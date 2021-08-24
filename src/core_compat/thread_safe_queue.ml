

type 'a t

type 'a node = { value : 'a; prev : 'a node Js.nullable ; next : 'a node Js.nullable }

(** [create ()] returns an empty queue. *)
external create : unit -> 'a t = "create" [@@bs.module "yallist"]

external length : _ t -> int = "length" [@@bs.get]
external enqueue : 'a t -> 'a -> unit = "push" [@@bs.send]

(** [dequeue_exn t] raises if [length t = 0].  The idiom for dequeueing a single element
    is:
    {[
      if length t > 0 then dequeue_exn t else ...
    ]}
    The idiom for dequeueing until empty is:
    {[
      while length t > 0 do
        let a = dequeue_exn t in
        ...
      done
    ]}
    These idioms work in the presence of threads because OCaml will not context switch
    between the [length t > 0] test and the call to [dequeue_exn].  Also, if one has only
    a single thread calling [dequeue_exn], then the idiom is obviously OK even in the
    presence of a context switch. *)
external dequeue_exn : 'a t -> 'a = "shift" [@@bs.send]

(** The queue maintains an internal pool of unused elements, which are used by [enqueue]
    and returned to the pool by [dequeue_exn].  [enqueue] creates a new element if the
    pool is empty.  Nothing shrinks the pool automatically.  One can call
    [clear_internal_pool] to clear the pool, so that all unused elements will be reclaimed
    by the garbage collector. *)

(* val clear_internal_pool : _ t -> unit *)

external tail : 'a t -> 'a node Js.nullable = "tail" [@@bs.send]

external head : 'a t -> 'a node Js.nullable = "head" [@@bs.send]

let rec sexp_of_node sexp_of_a node acc =
  match Js.Nullable.toOption node with
    | Some { value; prev } -> sexp_of_node sexp_of_a prev (sexp_of_a value :: acc)
    | None -> Ppx_sexp_conv_lib.Sexp.List acc

let drain q ~f =
  let f' v = f v [@bs] in
  let rec drain' node =
    match Js.Nullable.toOption node with
      | Some { value; next } -> f' value; drain' next
      | None -> ()
  in
  drain' (head q)

let sexp_of_t sexp_of_a q = sexp_of_node sexp_of_a (tail q) []
