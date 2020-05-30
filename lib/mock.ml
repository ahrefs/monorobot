open Base

let kind file =
  let basename = Caml.Filename.basename file in
  match String.split_on_chars basename ~on:[ '.' ] with
  | [ kind; _name; ext ] when String.equal ext "json" -> Some kind
  | _ -> None
