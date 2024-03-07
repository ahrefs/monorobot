module StringSet = Set.Make(String)

module StringMap = struct
  module Map = Map.Make(String)
  include Map
  type 'a t = 'a Map.t

  let empty : 'a t = Map.empty
  let to_list (l : 'a t) : (string * 'a) list = Map.to_seq l |> List.of_seq
  let of_list (m : (string * 'a) list) : 'a t = List.to_seq m |> Map.of_seq
  let wrap = of_list
  let unwrap = to_list
end

module Stringtbl = struct
  include Hashtbl
  type 'a t = (string, 'a) Hashtbl.t

  let empty () = Hashtbl.create 0
  let to_list (l : 'a t) : (string * 'a) list = Hashtbl.to_seq l |> List.of_seq
  let of_list (m : (string * 'a) list) : 'a t = List.to_seq m |> Hashtbl.of_seq
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
  match String.split_on_char '\n' s with
  | x :: _ -> x
  | [] -> s

let decode_string_pad s = Stre.rstrip ~chars:"= \n\r\t" s |> Base64.decode_exn ~pad:false

let http_request ?headers ?body meth path =
  let setup h =
    Curl.set_followlocation h true;
    Curl.set_maxredirs h 1
  in
  match%lwt Web.http_request_lwt ~setup ~ua:"monorobot" ~verbose:true ?headers ?body meth path with
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
  | x :: _ -> String.sub x 0 (Stre.common_prefix x (List.sort String.compare xs |> List.rev |> List.hd))

let sign_string_sha256 ~key ~basestring =
  Cstruct.of_string basestring |> Nocrypto.Hash.SHA256.hmac ~key:(Cstruct.of_string key) |> Hex.of_cstruct |> Hex.show

let dedup_and_sort ~compare l =
  List.fold_right
    (fun s (last, l) -> if Some s != last then (Some s, s :: l) else (last, l))
    (List.sort compare l)
    (None, [])
  |> snd