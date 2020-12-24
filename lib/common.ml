open Base
open Devkit

module StringMap = struct
  type 'a t = 'a Map.M(String).t

  let empty : 'a t = Map.empty (module String)

  let to_list (l : 'a t) : (string * 'a) list = Map.to_alist l

  let of_list (m : (string * 'a) list) : 'a t = Map.of_alist_exn (module String) m

  let wrap = of_list

  let unwrap = to_list
end

let fmt_error fmt = Printf.ksprintf (fun s -> Error s) fmt

let first_line s =
  match String.split ~on:'\n' s with
  | x :: _ -> x
  | [] -> s

let decode_string_pad s =
  String.rstrip ~drop:(List.mem [ '='; ' '; '\n'; '\r'; '\t' ] ~equal:Char.equal) s |> Base64.decode_string

let http_request ?headers ?body meth path =
  match%lwt Web.http_request_lwt ~ua:"monorobot" ~verbose:true ?headers ?body meth path with
  | `Ok s -> Lwt.return @@ Ok s
  | `Error e -> Lwt.return @@ Error e

let get_local_file path = try Ok (Std.input_file path) with exn -> fmt_error "%s" (Exn.to_string exn)

let write_to_local_file ~data path =
  try Ok (Devkit.Files.save_as path (fun oc -> Stdio.Out_channel.fprintf oc "%s" data))
  with exn -> fmt_error "%s" (Exn.to_string exn)
