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
