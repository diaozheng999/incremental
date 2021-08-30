open Base.Base
include Int

(* All [assert]s and other checks throughout the code are guarded by [if debug].  The
   DEBUG variable is set in the lib [incremental] and unset in the lib
   [incremental_debug], but apart from that they are identical.  Tests are run with both
   the production and debug lib, and users can choose to build with the debug library, if
   they suspect they found a bug in incremental. *)

let debug = false

let concat = String.concat
let tag name a sexp_of_a = (name, a) |> [%sexp_of: string * a]

module Step_function = Incremental_step_function

module Array = struct
  include Array

  (* Requires [len >= length t]. *)
  let realloc t ~len a =
    let new_t = create ~len a in
    Array.blit ~src:t ~src_pos:0 ~dst:new_t ~dst_pos:0 ~len:(length t);
    new_t
  ;;
end

module Uopt = struct
  include Uopt

  let unsafe_value = if debug then value_exn else unsafe_value
end

module Alarm_precision = Timing_wheel.Alarm_precision

external phys_same : 'a -> 'b -> bool = "%eq"

let with_return = With_return.with_return

let failwiths ?strict ~here message a sexp_of_a =
  let open Error in
  raise (create ?strict ~here message a sexp_of_a)

let const = Fn.const
