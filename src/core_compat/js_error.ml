open Base.Base

type t = {
  name : string;
  message : string;
  stack : string;
} [@@deriving sexp_of]

external of_js_exception : Caml_js_exceptions.t -> t = "%identity"

let raise_with_js_passthrough e =
  match Caml_js_exceptions.caml_as_js_exn e with
    | Some e -> Error.raise_s (sexp_of_t (Obj.magic e : t))
    | _ -> raise e

let raise_with_js_passthrough_m ~message e =
  match Caml_js_exceptions.caml_as_js_exn e with
  | Some e ->
    let error = [%sexp_of : string * t] (message, of_js_exception e) in
    Error.raise_s error
  | _ -> raise e
