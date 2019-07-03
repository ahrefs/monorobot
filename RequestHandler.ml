open Httpaf
let request_handler (_ : Unix.sockaddr) (reqd: Httpaf.Reqd.t) =
  let { Request.meth; _ } = Reqd.request reqd in
  match meth with
  | `POST ->
    let headers =
      Headers.of_list
        [ "Connection", "close" ]
    in
    Reqd.respond_with_string reqd (Response.create ~headers `OK) "";
  | meth ->
    let response_body =
      Printf.sprintf "%s is not an allowed method\n" (Method.to_string meth)
    in
    let headers = Headers.of_list [ "Connection", "close" ] in
    Reqd.respond_with_string reqd (Response.create ~headers `Method_not_allowed) response_body
;;

