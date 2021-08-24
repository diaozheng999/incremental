open Base.Base

type t = float [@@deriving hash, sexp]

external ( * ) : t -> t -> t = "%mulfloat"
external ( + ) : t -> t -> t = "%addfloat"
external ( - ) : t -> t -> t = "%subfloat"
external ( / ) : t -> t -> t = "%divfloat"
external ( // ) : t -> t -> float = "%divfloat"

external ( == ) : t -> t -> bool = "%eq"

let zero : t = 0.
external neg : t -> t = "%negfloat"
external abs : t -> t = "abs" [@@bs.val][@@bs.scope "Math"]

external is_finite : t -> bool = "isFinite" [@@bs.val]

let is_zero p = p == zero
external is_nan : t -> bool = "isNaN" [@@bs.val]
let is_inf f = not (is_finite f)

(** [apply t x] multiplies the percent [t] by [x], returning a float. *)
external apply : t -> float -> float = "%mulfloat"

(** [scale t x] scales the percent [t] by [x], returning a new [t]. *)
external scale : t -> float -> t = "%mulfloat"

(** [of_mult 5.] is 5x = 500% = 50_000bp *)
external of_mult : float -> t = "%identity"

external to_mult : t -> float = "%identity"

let of_percentage p = p /. 100.

let to_percentage p = p *. 100.

let of_bp f = f /. 10_000.

let to_bp t = t *. 10_000.

let of_bp_int i = of_bp (Float.of_int i)

let to_bp_int t = Float.to_int (to_bp t)
