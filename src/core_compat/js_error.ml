open Base.Base

type t = {
  name : string;
  message : string;
  stack : string;
} [@@deriving sexp_of]

exception Js_error of Sexp.t

let raise_with_js_passthrough e =
  match Caml_js_exceptions.caml_as_js_exn e with
    | Some e -> raise (Js_error (sexp_of_t (Obj.magic e : t)))
    | _ -> raise e
