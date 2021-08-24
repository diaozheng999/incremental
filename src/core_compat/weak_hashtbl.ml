(* JavaScript does not support weak references. *)

open Base.Base

type ('a, 'b) t = {
  entry_by_key : ('a, 'b Weak_ref.t) Hashtbl.t;
  keys_with_unused_data : 'a Thread_safe_queue.t;
  mutable thread_safe_run_when_unused_data : unit -> unit;
  finalization_registry : ('a Finalization_registry.t[@sexp.opaque]);
}

[@@deriving sexp_of]

module Using_hashable = struct
  external set_undefined_as_placeholder : 'a Finalization_registry.t
    = "#undefined"

  external set_finalization_registry :
    ('a, 'b) t -> 'a Finalization_registry.t -> unit = "finalization_registry"
    [@@bs.set]

  let create ?growth_allowed ?size hashable =
    let t =
      {
        entry_by_key =
          Hashtbl.create ?growth_allowed ?size (Hashable.to_key hashable);
        keys_with_unused_data = Thread_safe_queue.create ();
        thread_safe_run_when_unused_data = ignore;
        finalization_registry = set_undefined_as_placeholder;
      }
    in

    let cleanup =
     fun [@bs] key ->
      Thread_safe_queue.enqueue t.keys_with_unused_data key;
      t.thread_safe_run_when_unused_data ()
    in
    set_finalization_registry t (Finalization_registry.make cleanup);
    t
end

let create ?growth_allowed ?size m =
  Using_hashable.create ?growth_allowed ?size (Hashable.of_key m)

let set_run_when_unused_data t ~thread_safe_f =
  t.thread_safe_run_when_unused_data <- thread_safe_f

let remove t key = Hashtbl.remove t.entry_by_key key

(* In order for a call to [reclaim_space_for_keys_with_unused_data] to reclaim a key that
   was previously finalized, the weak pointer must have been cleared.  This relies on the
   fact that the OCaml garbage collector clears weaks and then runs finalizers. *)
let reclaim_space_for_keys_with_unused_data t =
  while Thread_safe_queue.length t.keys_with_unused_data > 0 do
    let key = Thread_safe_queue.dequeue_exn t.keys_with_unused_data in
    match Hashtbl.find t.entry_by_key key with
    | None -> ()
    | Some entry -> if Weak_ref.is_none entry then remove t key
  done

let mem t key =
  match Hashtbl.find t.entry_by_key key with
  | None -> false
  | Some entry -> not (Weak_ref.is_none entry)

let key_is_using_space t key = Hashtbl.mem t.entry_by_key key

let make_ref t key data =
  Finalization_registry.register t.finalization_registry data key;
  Weak_ref.make data ~ctx:t.finalization_registry

let set_data t key entry data =
  if Weak_ref.is_virtualized then Weak_ref.collect entry;
  make_ref t key data

let replace t ~key ~data =
  match Hashtbl.find t.entry_by_key key with
  | Some existing ->
      let data = set_data t key existing data in
      Hashtbl.set t.entry_by_key ~key ~data
  | None -> Hashtbl.set t.entry_by_key ~key ~data:(make_ref t key data)

let add_exn t ~key ~data =
  match Hashtbl.find t.entry_by_key key with
  | Some entry -> (
      match Weak_ref.deref entry with
      | Some _ ->
          let sexp_of_t : (_, _) t -> Sexplib0.Sexp.t = [%sexp_of: (_, _) t] in
          Error.create ~here:[%here] "Weak_hashtbl.add_exn of key in use" t
            sexp_of_t
          |> Error.raise
      | None -> Hashtbl.set t.entry_by_key ~key ~data:(make_ref t key data))
  | None ->
      Hashtbl.add t.entry_by_key ~key ~data:(make_ref t key data) |> ignore

let find t key =
  match Hashtbl.find t.entry_by_key key with
  | None -> None
  | Some entry -> Weak_ref.deref entry

let add_lazy t key value =
  let data = value () in
  add_exn t ~key ~data;
  data

let find_or_add t key ~default =
  let entry = find t key in
  match entry with None -> add_lazy t key default | Some entry -> entry
