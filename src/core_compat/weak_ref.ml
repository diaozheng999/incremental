open Base.Base
include Weak_ref_intf

type js_weak_ref_ctor

type 'a t

type unsafe_ptr

(** If WeakRef (JS TC39) is implemented *)
module Native : Unsafe with type t = unsafe_ptr = struct
  type t = unsafe_ptr

  let is_virtualized = false

  let sexp_of_t _ sexp =
    let open Sexp in
    List [ Atom "WeakRef"; sexp ]

  external make : 'a -> ctx:_ Finalization_registry.t -> t = "WeakRef"
    [@@bs.new]

  external deref : t -> 'a option = "deref" [@@bs.send]


  external collect : t -> unit = "%ignore"
end

(** Virtualized implementation fakes a strong pointer as a weak one, so objects
    will never be collected. *)
module Virtualized : Unsafe with type t = unsafe_ptr = struct
  type t = unsafe_ptr

  type 'a virtualized_pointer = {
    mutable value : 'a option;
    mutable is_collected : bool;
    held_by : (Finalization_registry.Virtualized.t[@sexp.opaque]);

  }
  [@@deriving sexp_of]

  let is_virtualized = true

  external pack : 'a virtualized_pointer -> t = "%identity"

  external unpack : t -> 'a virtualized_pointer = "%identity"

  external sexp_of_sexp : Sexp.t -> Sexp.t = "%identity"

  let make value ~ctx =
    let held_by = Finalization_registry.Virtualized.pack_t ctx in
    pack { value = Some value; is_collected = false; held_by }

  let sexp_of_t ptr sexp =
    let unpacked = unpack ptr in
    if Belt.Option.isSome unpacked.value then unpacked.value <- Some sexp;
    sexp_of_virtualized_pointer sexp_of_sexp unpacked

  let deref p =
    match unpack p with { value; is_collected = false } -> value | _ -> None

  let collect t =
    let unpacked = unpack t in
    let value = unpacked.value in
    unpacked.value <- None;
    unpacked.is_collected <- true;
    Finalization_registry.Virtualized.virtualized_execute_finalizers_for
      unpacked.held_by value

end

external js_weak_ref_ctor : js_weak_ref_ctor = "WeakRef" [@@bs.val]

let decide (type t) =
  let native : (module Unsafe with type t = unsafe_ptr) = (module Native) in
  let virtualized : (module Unsafe with type t = unsafe_ptr) =
    (module Virtualized)
  in
  if Js.typeof js_weak_ref_ctor != "undefined" then native
  else
    let () =
      Js.Console.warn
        ("WeakRef is not implemented in this platform. "
       ^ "All weak references will be treated as strong.")
    in
    virtualized

module Unsafe = (val decide)

let is_virtualized = Unsafe.is_virtualized

let make = Obj.magic Unsafe.make

let deref = Obj.magic Unsafe.deref

let collect = Obj.magic Unsafe.collect

let is_none ptr = Belt.Option.isNone (deref ptr)

let sexp_of_t sexp_of_a a_ptr =
  let sexp = deref a_ptr |> [%sexp_of: a option] in
  let ptr : Unsafe.t = Obj.magic a_ptr in
  Unsafe.sexp_of_t ptr sexp
