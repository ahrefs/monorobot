open Base

module StringMap = struct
  type 'a t = 'a Map.M(String).t

  let empty : 'a t = Map.empty (module String)
  let to_list (l : 'a t) : (string * 'a) list = Map.to_alist l
  let of_list (m : (string * 'a) list) : 'a t = Map.of_alist_exn (module String) m
  let wrap = of_list
  let unwrap = to_list
end

module Stringtbl = struct
  include Hashtbl

  type 'a t = 'a Hashtbl.M(String).t

  let empty () = Hashtbl.create (module String)
  let to_list (l : 'a t) : (string * 'a) list = Hashtbl.to_alist l
  let of_list (m : (string * 'a) list) : 'a t = Hashtbl.of_alist_exn (module String) m
  let wrap = of_list
  let unwrap = to_list
end

module Re2 = struct
  include Re2

  let wrap s = create_exn s
  let unwrap = Re2.to_string
end

open Devkit

let fmt_error fmt = Printf.ksprintf (fun s -> Error s) fmt

let first_line s =
  match String.split ~on:'\n' s with
  | x :: _ -> x
  | [] -> s

let decode_string_pad s =
  String.rstrip ~drop:(List.mem [ '='; ' '; '\n'; '\r'; '\t' ] ~equal:Char.equal) s |> Base64.decode_exn ~pad:false

let http_request ?headers ?body meth path =
  match%lwt Web.http_request_lwt ~ua:"monorobot" ~verbose:true ?headers ?body meth path with
  | `Ok s -> Lwt.return @@ Ok s
  | `Error e -> Lwt.return @@ Error e

let get_local_file path = try Ok (Std.input_file path) with exn -> fmt_error "%s" (Exn.to_string exn)

let write_to_local_file ~data path =
  try Ok (Devkit.Files.save_as path (fun oc -> Stdio.Out_channel.fprintf oc "%s" data))
  with exn -> fmt_error "%s" (Exn.to_string exn)

let longest_common_prefix xs =
  match xs with
  | [] -> ""
  | [ x ] -> x
  | x :: _ -> String.sub x ~pos:0 ~len:(Stre.common_prefix x (List.sort xs ~compare:String.compare |> List.last_exn))

let sign_string_sha256 ~key ~basestring =
  Cstruct.of_string basestring |> Nocrypto.Hash.SHA256.hmac ~key:(Cstruct.of_string key) |> Hex.of_cstruct |> Hex.show
