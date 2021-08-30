type +'a t

external create_exn : 'a -> 'a t = "%identity"

external value : 'a t -> 'a = "valueOf" [@@bs.send]

external create_int : int -> int t = "Number" [@@bs.new]

external create_string : string -> string t = "String" [@@bs.new]

external create_float : float -> float t = "Number" [@@bs.new]

external create_bool : bool -> bool t = "Boolean" [@@bs.new]

(* Melange does not have a type-safe interface for BigInt and Symbol.
   To avoid confusion over user-implemented interfaces, we do not restrict
   the type at OCaml-end.
   
   The correct types should be:
   val create_bigint : bigint -> bigint t
   val create_symbol : symbol -> symbol t
   *)

external unsafe_create_bigint : 'a -> 'a t = "BigInt" [@@bs.new]

external unsafe_create_symbol : 'a -> 'a t = "Symbol" [@@bs.new]

external create : 'a -> 'a t = "Object" [@@bs.val]

let sexp_of_t sexp_of_a t = sexp_of_a (value t)
