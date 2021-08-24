open Base.Base

(** Exposing that this is a float allows for more optimization. E.g. compiler can
    optimize some local refs and not box them.
*)
type t = private float [@@deriving hash, sexp]

external ( * ) : t -> t -> t = "%mulfloat"
external ( + ) : t -> t -> t = "%addfloat"
external ( - ) : t -> t -> t = "%subfloat"
external ( / ) : t -> t -> t = "%divfloat"
external ( // ) : t -> t -> float = "%divfloat"
val zero : t
external neg : t -> t = "%negfloat"
external abs : t -> t = "abs" [@@bs.val][@@bs.scope "Math"]


val is_zero : t -> bool
external is_nan : t -> bool = "isNaN" [@@bs.val]
val is_inf : t -> bool

(** [apply t x] multiplies the percent [t] by [x], returning a float. *)
external apply : t -> float -> float = "%mulfloat"

(** [scale t x] scales the percent [t] by [x], returning a new [t]. *)
external scale : t -> float -> t = "%mulfloat"

(** [of_mult 5.] is 5x = 500% = 50_000bp *)
external of_mult : float -> t = "%identity"

external to_mult : t -> float = "%identity"

(** [of_percentage 5.] is 5% = 0.05x = 500bp *)
val of_percentage : float -> t

val to_percentage : t -> float

(** [of_bp 5.] is 5bp = 0.05% = 0.0005x *)
val of_bp : float -> t

val to_bp : t -> float
val of_bp_int : int -> t

(** rounds down *)
val to_bp_int : t -> int
