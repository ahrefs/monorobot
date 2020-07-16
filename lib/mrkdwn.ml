open Omd
open Base

let escape_url_chars str =
  let repl c = String.substr_replace_all ~pattern:(Char.to_string c) ~with_:(Printf.sprintf "\\%c" c) in
  str |> repl '<' |> repl '>' |> repl '|'

let rec transform_list = List.map ~f:transform

and transform_flatten = List.map ~f:transform_list

and surround s t =
  let t = to_markdown @@ transform_list t in
  Raw (Printf.sprintf "%s%s%s" s t s)

and transform = function
  | H1 t -> H1 (transform_list t)
  | H2 t -> H2 (transform_list t)
  | H3 t | H4 t | H5 t | H6 t -> Paragraph [ Bold (transform_list t) ]
  | Paragraph t -> Paragraph (transform_list t)
  | Emph t -> surround "_" t
  | Bold t -> surround "*" t
  | Ul ts -> Ul (transform_flatten ts)
  | Ol ts -> Ol (transform_flatten ts)
  | Ulp ts -> Ulp (transform_flatten ts)
  | Olp ts -> Olp (transform_flatten ts)
  | Url (href, label, title) ->
    let label = escape_url_chars @@ to_markdown @@ transform_list label in
    let title = if String.length title > 0 then Printf.sprintf "%s - " @@ escape_url_chars title else title in
    Raw (Printf.sprintf "<%s%s|%s>" title label href)
  | Html _ as e -> Raw (Printf.sprintf "`%s`" @@ to_markdown [ e ])
  | Html_comment _ -> Br
  | Html_block _ as e -> Code_block ("html", to_markdown [ e ])
  | Blockquote t -> Blockquote (transform_list t)
  | Img (alt, src, title) -> Url (src, [ Text alt ], title)
  | (Text _ | Code _ | Code_block _ | Br | Hr | NL | Ref _ | Img_ref _ | Raw _ | Raw_block _ | X _) as e -> e

let of_doc t = transform_list t

let to_mrkdwn doc = to_markdown @@ of_doc doc

let mrkdwn_of_markdown str = to_mrkdwn @@ of_string str
