open Base.Base

type t = {
  name : string;
  message : string;
  stack : string;
} [@@deriving sexp_of]

let raise_with_js_passthrough e =
  match Caml_js_exceptions.caml_as_js_exn e with
    | Some e -> Error.raise_s (sexp_of_t (Obj.magic e : t))
    | _ -> raise e
