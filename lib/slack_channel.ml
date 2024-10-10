type t =
  | User of string
  | Channel of string

let to_channel c = Channel c
let to_user u = User u

let string_val c =
  match c with
  | Channel s -> s
  | User s -> s

let compare c1 c2 =
  match c1, c2 with
  | Channel c1, Channel c2 -> String.compare c1 c2
  | User u1, User u2 -> String.compare u1 u2
  | _ -> failwith "Cannot compare channels of different types"
