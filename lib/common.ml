open Base

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
  let rec strip_padding i =
    if i < 0 then ""
    else (
      match s.[i] with
      | '=' | '\n' | '\r' | '\t' | ' ' -> strip_padding (i - 1)
      | _ -> String.sub s ~pos:0 ~len:(i + 1)
    )
  in
  Base64.decode_string @@ strip_padding (String.length s - 1)
