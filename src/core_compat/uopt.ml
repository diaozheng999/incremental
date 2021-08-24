type 'a t

external is_none : 'a t -> bool = "#is_nullable"

external none : 'a t = "#undefined"

let is_some v = not (is_none v)

external unsafe_value : 'a t -> 'a = "%identity"

external some : 'a -> 'a t = "%identity"

external as_opt : 'a t -> 'a option = "#nullable_to_opt"

let value_exn v =
  if is_none v then failwith "Uopt returned none." else unsafe_value v

let invariant invariant_a a_opt =
  if is_none a_opt then () else invariant_a (unsafe_value a_opt)

let sexp_of_t sexp_of_a a_opt =
  Ppx_sexp_conv_lib.Conv.sexp_of_option sexp_of_a (as_opt a_opt)

external to_option : 'a t -> 'a option = "#nullable_to_opt"
