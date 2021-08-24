open Base.Base

module type Weak_ref = sig
  type 'a t [@@deriving sexp_of]

  val make : 'a Heap_block.t -> ctx:_ Finalization_registry.t -> 'a t 

  val deref : 'a t -> 'a Heap_block.t option

  val is_virtualized : bool

  val collect : 'a t -> unit

  val is_none : 'a t -> bool
end

module type Unsafe = sig
  type t

  val sexp_of_t : t -> Sexp.t -> Sexp.t
  
  val make : 'a Heap_block.t -> ctx:_ Finalization_registry.t -> t

  val deref : t -> 'a Heap_block.t option

  val is_virtualized : bool

  val collect : t -> unit
end
