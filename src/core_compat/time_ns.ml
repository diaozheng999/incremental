open Base.Base

module Span = struct
  type t = float [@@deriving sexp_of]

  let t_of_sexp : Sexp.t -> t = Obj.magic float_of_sexp

  external of_int_ns : int -> t = "%identity"

  external ( <= ) : t -> t -> bool = "%lessequal"

  external ( < ) : t -> t -> bool = "%lessthan"

  let max_value_representable : t = 9007199254740991. 

  let epoch : t = 0.

  let zero : t = 0.

  let nanosecond = 1.

  external of_float : float -> t = "%identity"

  let of_int63_ns v = Int63.(v |> to_float)

  let to_int63_ns v = Int63.of_float v

  external to_float : t -> float = "%identity"
    
  external add : t -> t -> t = "%addfloat"

  external sub : t -> t -> t = "%subfloat"

  external ( - ) : t -> t -> t = "%subfloat"

  external div : t -> t -> float = "%divfloat"

  external scale_float : t -> float -> t = "%mulfloat"

  let is_positive v = Base.Base.Float.(v >= 0.)

  let scale_int63 f t = f *. Int63.to_float t
end

type t = Span.t [@@deriving sexp_of]

external ( < ) : t -> t -> bool = "%lessthan"

external ( <= ) : t -> t -> bool = "%lessequal"

external ( >= ) : t -> t -> bool = "%greaterequal"

external ( > ) : t -> t -> bool = "%greaterthan"

external ( = ) : t -> t -> bool = "%eq"

external equal : t -> t -> bool = "%eq"

external compare : t -> t -> int = "%compare"

let max_value_representable = Span.max_value_representable

let epoch = Span.epoch

let to_float_since_epoch = Span.to_float

let to_int63_ns_since_epoch = Span.to_int63_ns

let of_float_since_epoch = Span.of_float

let of_int63_ns_since_epoch = Span.of_int63_ns

let min_value_for_1us_rounding = 0.

external max : t -> t -> t = "max" [@@bs.val] [@@bs.scope "Math"]

external add : t -> Span.t -> t = "%addfloat"

external sub : t -> Span.t -> t = "%subfloat"

external diff : t -> t -> Span.t = "%subfloat"

external round : t -> t = "round" [@@bs.val][@@bs.scope "Math"]

let[@cold] raise_next_multiple_got_nonpositive_interval interval =
  Error.create
    ~here:[%here]
    "Time_ns.next_multiple got nonpositive interval"
    interval
    [%sexp_of: Span.t]
  |> Error.raise

let next_multiple_internal ~can_equal_after ~base ~after ~interval =
  if Span.( <= ) interval Span.zero
  then raise_next_multiple_got_nonpositive_interval interval;
  let base_to_after = diff after base in
  if Span.( < ) base_to_after Span.zero
  then base (* [after < base], choose [k = 0]. *)
  else (
    let next = add base (Span.scale_float interval (Span.div base_to_after interval)) in
    if next > after || (can_equal_after && next = after) then next else add next interval)
;;

let next_multiple ?(can_equal_after = false) ~base ~after ~interval () =
  next_multiple_internal ~can_equal_after ~base ~after ~interval

let to_time_float_round_nearest_microsecond t = (t /. 1000. |> round) /. 1000.

let of_time_float_round_nearest_microsecond f = (f *. 1000. |> round) *. 1000.
