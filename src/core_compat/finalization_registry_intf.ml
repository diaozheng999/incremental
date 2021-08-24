module type Unsafe = sig
  type t

  val is_virtualized : bool

  val make : (('a -> unit)[@bs]) -> t

  val register : t -> _ -> 'a -> unit

  val unregister : t -> _ -> unit
end

module type Finalization_registry = sig
  type 'a t

  val is_virtualized : bool

  val make : (('a -> unit)[@bs]) -> 'a t

  val register : 'a t -> _ -> 'a -> unit

  val unregister : 'a t -> _ -> unit
end
