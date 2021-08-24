include Base.Ppx_compare_lib

external phys_equal : 'a -> 'a -> bool = "is" [@@bs.val] [@@bs.scope "Object"]

module Builtin = struct
  include Base.Ppx_compare_lib.Builtin

  external equal_float : float -> float -> bool = "%equal"

  external equal_int : int -> int -> bool = "%equal"

  external equal_bool : bool -> bool -> bool = "%equal"

  external equal_char : char -> char -> bool = "%equal"

  external equal_int32 : int32 -> int32 -> bool = "%equal"

  external equal_nativeint : int32 -> int32 -> bool = "%equal"

  external equal_string : string -> string -> bool = "%equal"

  external equal_unit : unit -> unit -> bool = "%equal"

  external compare_bool : bool -> bool -> int = "%compare"

  external compare_char : char -> char -> int = "%compare"

  external compare_float : float -> float -> int = "%compare"

  external compare_int : int -> int -> int = "%compare"

  external compare_int32 : int32 -> int32 -> int = "%compare"

  external compare_nativeint : int32 -> int32 -> int = "%compare"

  external compare_string : string -> string -> int = "%compare"

  external compare_unit : unit -> unit -> int = "%compare"

  let equal_int64 : int64 -> int64 -> bool =
    [%raw "(a, b) => a[0]===b[0] && a[1]===b[1]"]
end
