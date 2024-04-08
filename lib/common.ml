module StringSet = Set.Make (String)

module StringMap = struct
  include Map.Make (String)

  let to_list (l : 'a t) : (string * 'a) list = to_seq l |> List.of_seq
  let of_list (m : (string * 'a) list) : 'a t = List.to_seq m |> of_seq
  let wrap = of_list
  let unwrap = to_list

  let of_list_multi (m : (string * 'a) list) : 'a list t =
    let update_f v = function
      | None -> Some [ v ]
      | Some vs -> Some (v :: vs)
    in
    List.fold_right (fun (k, v) b -> update k (update_f v) b) m empty
end

module Stringtbl = struct
  include Hashtbl

  type 'a t = (string, 'a) Hashtbl.t

  let empty () = Hashtbl.create 1
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
  try Ok (Devkit.Files.save_as path (fun oc -> Printf.fprintf oc "%s" data))
  with exn -> fmt_error "%s" (Exn.to_string exn)

let sign_string_sha256 ~key ~basestring =
  Cstruct.of_string basestring |> Nocrypto.Hash.SHA256.hmac ~key:(Cstruct.of_string key) |> Hex.of_cstruct |> Hex.show
