module StringSet = struct
  include Set.Make (String)

  let to_list set : string list = elements set
  let wrap = of_list
  let unwrap = to_list
end

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
