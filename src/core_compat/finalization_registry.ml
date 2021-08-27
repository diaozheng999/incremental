open Finalization_registry_intf

type 'a t

type unsafe_finalization_registry

type js_finalization_registry_ctor

module Native : Unsafe with type t = unsafe_finalization_registry = struct
  type t = unsafe_finalization_registry

  let is_virtualized = false
  external make : (('a -> unit)[@bs]) -> t = "FinalizationRegistry" [@@bs.new]

  external register : t -> _ -> 'a -> unit = "register" [@@bs.send]

  external unregister : t -> _ -> unit = "unregister" [@@bs.send]
end

module Virtualized = struct
  type 'a t_ = 'a t

  type t = unsafe_finalization_registry

  type 'a weak_map

  type 'a virtualized_finalization_registry = {
    objects : 'a weak_map;
    finalizer : ('a -> unit[@bs]);
  }

  external pack : 'a virtualized_finalization_registry -> t = "%identity"

  external pack_t : 'a t_ -> t = "%identity"

  external unpack : t -> 'a virtualized_finalization_registry = "%identity"

  external make_weak_map : unit -> 'a weak_map = "WeakMap" [@@bs.new]

  external get_from_map : 'a weak_map -> _ -> 'a option = "get" [@@bs.send]


  external set_map : 'a weak_map -> _ -> 'a -> unit = "set" [@@bs.send]

  external delete_from_map : 'a weak_map -> _ -> bool = "delete" [@@bs.send]

  let is_virtualized = true
  let make finalizer = pack { objects = make_weak_map (); finalizer }

  let register registry obj held_value =
    let { objects; _ } = unpack registry in
    set_map objects obj held_value

  let unregister registry obj =
    let { objects; _ } = unpack registry in
    let (_ : bool) = delete_from_map objects obj in
    ()

  let virtualized_execute_finalizers_for registry obj =
    let { objects; finalizer } = unpack registry in
    match get_from_map objects obj with
    | None -> ()
    | Some held_value ->
        finalizer held_value [@bs];
        let (_ : bool) = delete_from_map objects obj in
        ()
end

external js_finalization_registry_ctor : js_finalization_registry_ctor
  = "FinalizationRegistry"
  [@@bs.val]


let decide =
  let native : (module Unsafe with type t = unsafe_finalization_registry) =
    (module Native)
  in

  let virtualized : (module Unsafe with type t = unsafe_finalization_registry) =
    (module Virtualized)
  in

  if Js.typeof js_finalization_registry_ctor != "undefined" then native
  else
    let () =
      Js.Console.warn
        "FinalizationRegistry is not implemented in this platform. "
    in
    virtualized

module Unsafe = (val decide)

module Registry : Finalization_registry with type 'a t := 'a t = struct
  type 'a t

  let make = Obj.magic Unsafe.make

  let register = Obj.magic Unsafe.register

  let unregister = Obj.magic Unsafe.unregister

  let is_virtualized = Unsafe.is_virtualized

end

include Registry
