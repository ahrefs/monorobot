open Omd
open Base
open Devkit

let log = Log.from "mrkdwn"

let rec transform e =
  match e with
  | H1 t -> H1 (transform_list t)
  | H2 t -> H2 (transform_list t)
  | H3 t | H4 t | H5 t | H6 t -> Paragraph [ Bold (transform_list t) ]
  | Paragraph t -> Paragraph (transform_list t)
  | Text _ as e -> e
  | Emph t -> surround "_" t
  | Bold t -> surround "*" t
  | Ul ts -> Ul (transform_list2 ts)
  | Ol ts -> Ol (transform_list2 ts)
  | Ulp ts -> Ulp (transform_list2 ts)
  | Olp ts -> Olp (transform_list2 ts)
  | Url (href, label, title) ->
    let label = to_markdown @@ transform_list label in
    let title = if String.length title > 0 then Printf.sprintf "%s - " title else title in
    Raw (Printf.sprintf "<%s%s|%s>" title label href)
  | (Html _ | Html_comment _) as e -> Raw (Printf.sprintf "`%s`" @@ to_markdown [ e ])
  | Html_block _ as e -> Code_block ("html", to_markdown [ e ])
  | Blockquote t -> Blockquote (transform_list t)
  | Img (alt, src, title) -> Url (src, [ Text alt ], title)
  | (Code _ | Code_block _ | Br | Hr | NL | Ref _ | Img_ref _ | Raw _ | Raw_block _ | X _) as e -> e

and transform_list = List.map ~f:transform

and transform_list2 = List.map ~f:transform_list

and surround s t =
  let t = to_markdown @@ transform_list t in
  Raw (Printf.sprintf "%s%s%s" s t s)

let of_doc (t : t) = transform_list t

let to_mrkdwn doc = to_markdown @@ of_doc doc

let mrkdwn_of_markdown str = to_mrkdwn @@ of_string str
