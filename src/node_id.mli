(** A module internal to Incremental.  Users should see {!Incremental_intf}.

    Node ids are consecutive integers assigned to nodes as they are created. *)

open! Base.Base
open! Import

type t = private int [@@deriving compare, sexp_of]

include Invariant.S with type t := t

module Hash_set : sig
    type set
    external create : unit -> set = "Set" [@@bs.new]
    external mem : set -> t -> bool = "has" [@@bs.send]
    external add : set -> t -> unit = "add" [@@bs.send]
end

val next : unit -> t
val to_string : t -> string
