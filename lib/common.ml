open Base
open Devkit

let fmt_error fmt = Printf.ksprintf (fun s -> Error s) fmt

let first_line s =
  match String.split ~on:'\n' s with
  | x :: _ -> x
  | [] -> s

module Tristate : Atdgen_runtime.Json_adapter.S = struct
  let normalize = function
    | `Bool true -> `String "true"
    | `Bool false -> `String "false"
    | x -> x

  let restore = function
    | `String "true" -> `Bool true
    | `String "false" -> `Bool false
    | x -> x
end

let decode_string_pad s =
  String.rstrip ~drop:(List.mem [ '='; ' '; '\n'; '\r'; '\t' ] ~equal:Char.equal) s |> Base64.decode_string

let http_get ?headers path =
  match%lwt Web.http_request_lwt ~ua:"monorobot" ~verbose:true ?headers `GET path with
  | `Ok s -> Lwt.return @@ Ok s
  | `Error e -> Lwt.return @@ Error e

let http_post ~path ~data =
  let body = `Raw ("application/json", data) in
  match%lwt Web.http_request_lwt ~verbose:true ~body `POST path with
  | `Ok res -> Lwt.return @@ Ok res
  | `Error e -> Lwt.return @@ Error e

let get_local_file path =
  try%lwt
    let%lwt data = Lwt_io.with_file ~mode:Lwt_io.input path (fun ic -> Lwt_io.read ic) in
    Lwt.return @@ Ok data
  with exn -> Lwt.return @@ Error (Exn.str exn)

let write_to_local_file ~path ~data =
  try%lwt
    let%lwt () =
      Lwt_io.with_file ~flags:[ O_CREAT; O_WRONLY; O_TRUNC ] ~mode:Lwt_io.output path (fun oc -> Lwt_io.write oc data)
    in
    Lwt.return @@ Ok ()
  with exn -> Lwt.return @@ Error (Exn.str exn)
