open Omd
open Base

let escape_url_chars = Staged.unstage @@ String.Escaping.escape ~escapeworthy:[ '<'; '>'; '|' ] ~escape_char:'\\'

(* https://api.slack.com/reference/surfaces/formatting#escaping *)
let escape_mrkdwn =
  String.concat_map ~f:(function
    | '<' -> "&lt;"
    | '>' -> "&gt;"
    | '&' -> "&amp;"
    | c -> String.make 1 c)

(** Unescape markdown characters escaped because "any ASCII punctuation
    character may be backslash-escaped"
    https://spec.commonmark.org/0.30/#backslash-escapes

    This pertains to '\\', '[', ']', '(', ')', '`', '*' unconditionally,
    and '.', '-', '+', '!', '<', '>', '#' depending on chars before/after.
    Argument escapeworthy_map can be left blank because escaped chars are
    unescaped to themselves. *)
let unescape_omd = Staged.unstage @@ String.Escaping.unescape_gen_exn ~escapeworthy_map:[] ~escape_char:'\\'

(** Escape the `escape_char` '\\' for use with `unescape_omd` later *)
let escape_omd = Staged.unstage @@ String.Escaping.escape_gen_exn ~escapeworthy_map:[] ~escape_char:'\\'

let transform_text = escape_mrkdwn

(** `Omd.to_markdown` escapes backslash (and other applicable chars) in
    `Text` elements but not `Code` elements, so do the same for the latter so
    that `unescape_omd` can apply uniformly to the whole mrkdwn string later *)
let transform_code s = escape_omd @@ escape_mrkdwn s

let rec transform_list = List.map ~f:transform

and transform_flatten = List.map ~f:transform_list

and surround s t =
  let t = to_markdown @@ transform_list t in
  Raw (Printf.sprintf "%s%s%s" s t s)

(** massage markdown AST so that rendered result looks like slack mrkdwn *)
and transform = function
  | H1 t | H2 t | H3 t | H4 t | H5 t | H6 t -> Paragraph (transform_list [ Bold t ])
  | Paragraph t -> Paragraph (transform_list t)
  | Emph t -> surround "_" (transform_list t)
  | Bold t -> surround "*" (transform_list t)
  | Ul ts -> Ul (transform_flatten ts)
  | Ol ts -> Ol (transform_flatten ts)
  | Ulp ts -> Ulp (transform_flatten ts)
  | Olp ts -> Olp (transform_flatten ts)
  | Url (href, label, title) ->
    let label = escape_url_chars @@ to_markdown @@ transform_list label in
    let title = if String.length title > 0 then Printf.sprintf "%s - " @@ escape_url_chars title else title in
    Raw (Printf.sprintf "<%s|%s%s>" href title label)
  | Html _ as e -> Raw (Printf.sprintf "`%s`" @@ to_markdown [ e ])
  | Html_comment _ -> Br
  | Html_block _ as e -> Code_block ("", to_markdown [ e ])
  | Blockquote t -> Blockquote (transform_list t)
  | Img (alt, src, title) -> transform @@ Url (src, [ Text alt ], title)
  | Code_block (_, str) -> Code_block ("", transform_code str)
  | Code (_, str) -> Code ("", transform_code str)
  | Text s -> Text (transform_text s)
  | (Br | Hr | NL | Ref _ | Img_ref _ | Raw _ | Raw_block _ | X _) as e -> e

(** Translates omd AST to a Slack mrkdwn string. Code heavily adapted
    from omd 1.3.1 source.
    https://github.com/ocaml/omd/blob/1.3.1/src/omd_backend.ml#L872
*)
let mrkdwn_of_md md =
  let b = Buffer.create 128 in
  let rec f tl = function
    | X _ -> loop tl
    | Blockquote _q -> loop tl
    | Ref (_rc, _name, _text, _fallback) -> loop tl
    | Img_ref (_rc, _name, _text, _fallback) -> loop tl
    | Paragraph _md -> loop tl
    | Img (_alt, _src, _title) -> loop tl
    | Text _t -> loop tl
    | Raw _t -> loop tl
    | Raw_block _t -> loop tl
    | Emph _md -> loop tl
    | Bold _md -> loop tl
    | Ul _l -> loop tl
    | Ol _l -> loop tl
    | Ulp _l -> loop tl
    | Olp _l -> loop tl
    | Code_block (_lang, _c) -> loop tl
    | Code (_lang, _c) -> loop tl
    | Br -> loop tl
    | Hr -> loop tl
    | Html (_tagname, _attrs, _body) -> loop tl
    | Html_block (_tagname, _attrs, _body) -> loop tl
    | Html_comment _s -> loop tl
    | Url (_href, _s, _title) -> loop tl
    | H1 _md -> loop tl
    | H2 _md -> loop tl
    | H3 _md -> loop tl
    | H4 _md -> loop tl
    | H5 _md -> loop tl
    | H6 _md -> loop tl
    | NL -> loop tl
  and loop = function
    | hd :: tl -> f tl hd
    | [] -> ()
  in
  loop md;
  Buffer.contents b

let mrkdwn_of_markdown str = unescape_omd @@ to_markdown @@ transform_list @@ of_string str
