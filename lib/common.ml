open Base

let first_line s =
  match String.split ~on:'\n' s with
  | x :: _ -> x
  | [] -> s
