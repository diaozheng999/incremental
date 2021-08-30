open Base.Base
open! Import

type 'a t =
  (* We specialize some cutoffs to avoid an indirect function call; in particular we
     specialize the default (and hence overwhelmingly common) case of physical
     equality. *)
  | Always
  | Never
  | Phys_equal
  | Compare of ('a -> 'a -> int)
  | Equal of ('a -> 'a -> bool)
  | F of (old_value:'a -> new_value:'a -> bool)
  | Js_uncurried of (('a -> 'a -> bool)[@bs])
  | Js_equal
  | Js_is
[@@deriving sexp_of]

let invariant _ t =
  Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
      match t with
      | Always | Never | Phys_equal | Compare _ | Equal _ | F _ | Js_uncurried _ 
      | Js_equal | Js_is -> ())
;;

external js_equal : 'a -> 'a -> bool = "%eq"

external js_is : 'a -> 'a -> bool = "is" [@@bs.val][@@bs.scope "Object"]

let create f = F f
let of_compare f = Compare f
let of_equal f = Equal f
let never = Never
let always = Always
let poly_equal = Equal Poly.equal

let should_cutoff t ~old_value ~new_value =
  match t with
  | Phys_equal -> phys_equal old_value new_value
  | Never -> false
  | Always -> true
  | Compare f -> f old_value new_value = 0
  | Equal f -> f old_value new_value
  | F f -> f ~old_value ~new_value
  | Js_uncurried f -> f old_value new_value [@bs]
  | Js_equal -> js_equal old_value new_value
  | Js_is -> js_is old_value new_value
;;

let equal t1 t2 =
  match t1, t2 with
  | Always, Always -> true
  | Always, _ -> false
  | Never, Never -> true
  | Never, _ -> false
  | Phys_equal, Phys_equal -> true
  | Phys_equal, _ -> false
  | Compare f1, Compare f2 -> phys_equal f1 f2
  | Compare _, _ -> false
  | Equal f1, Equal f2 -> phys_equal f1 f2
  | Equal _, _ -> false
  | F f1, F f2 -> phys_equal f1 f2
  | F _, _ -> false
  | Js_uncurried f1, Js_uncurried f2 -> phys_equal f1 f2
  | Js_uncurried _, _ -> false
  | Js_equal, Js_equal -> true
  | Js_equal, _ -> false
  | Js_is, Js_is -> true
  | Js_is, _ -> false
;;

let phys_equal = Phys_equal

module Js = struct
  let uncurried f = Js_uncurried f

  let is = Js_is

  let equal = Js_equal

end
