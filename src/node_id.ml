open Base.Base
open! Import
include Int

module Hash_set = struct
  type set
  external create : unit -> set = "Set" [@@bs.new]
  external mem : set -> t -> bool = "has" [@@bs.send]
  external add : set -> t -> unit = "add" [@@bs.send]
end

let invariant t = assert (t >= 1)

let next =
  let r = ref 0 in
  fun () ->
    incr r;
    !r
;;
