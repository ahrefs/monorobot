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
  let references : ref_container option ref = ref None in
  let rec f tl = function
    | X _ -> loop tl
    | Blockquote _q -> (* todo *) loop tl
    | Ref (rc, _name, _text, fallback) | Img_ref (rc, _name, _text, fallback) ->
      (* [rc] stores refs from whole document, so it's enough to record just the
         first encounter
      *)
      if Option.is_empty !references then references := Some rc;
      (* [fallback#to_string] renders as
         [<text>][<name>] for Ref, e.g., [interesting fact][1]
         and
         ![<text>][<name>] for Img_ref, e.g., ![image of cat][1]
      *)
      loop (Raw fallback#to_string :: tl)
    | Paragraph _md -> (* todo *) loop tl
    | Img (alt, src, title) -> loop (Url (src, [ Text alt ], title) :: tl)
    | Text t ->
      Buffer.add_string b @@ escape_mrkdwn t;
      loop tl
    | Raw s ->
      Buffer.add_string b s;
      loop tl
    | Raw_block s ->
      Buffer.add_char b '\n';
      Buffer.add_string b s;
      Buffer.add_char b '\n';
      loop tl
    | Emph md' ->
      Buffer.add_string b "_";
      loop md';
      Buffer.add_string b "_";
      loop tl
    | Bold md' ->
      Buffer.add_string b "*";
      loop md';
      Buffer.add_string b "*";
      loop tl
    | Ul _l -> (* todo *) loop tl
    | Ol _l -> (* todo *) loop tl
    | Ulp _l -> (* todo *) loop tl
    | Olp _l -> (* todo *) loop tl
    | Code_block (_lang, _c) -> (* todo *) loop tl
    | Code (_lang, c) ->
      Buffer.add_char b '`';
      Buffer.add_string b (escape_mrkdwn c);
      Buffer.add_char b '`';
      loop tl
    | Hr ->
      Buffer.add_string b "* * *\n";
      loop tl
    | Html (tagname, _attrs, body) ->
      Printf.bprintf b "`&lt;%s&gt;" tagname;
      Buffer.add_string b (escape_mrkdwn @@ to_html body);
      Printf.bprintf b "&lt;/%s&gt;`" tagname;
      loop tl
    | Html_block (_tagname, _attrs, _body) -> (* todo *) loop tl
    | Html_comment _s -> loop tl
    | Url (href, s, title) ->
      Buffer.add_char b '<';
      Buffer.add_string b href;
      Buffer.add_char b '|';
      if String.length title > 0 then Printf.bprintf b "%s - " @@ escape_mrkdwn title;
      loop s;
      Buffer.add_char b '>';
      loop tl
    | H1 md' | H2 md' | H3 md' | H4 md' | H5 md' | H6 md' -> loop (Paragraph (transform_list [ Bold md' ]) :: tl)
    | NL | Br ->
      (* the string "\n" renders NL
         the string "\\n" (backslash-newline) renders Br
      *)
      Buffer.add_char b '\n';
      loop tl
  and loop = function
    | hd :: tl -> f tl hd
    | [] -> ()
  in
  (* print the document *)
  loop md;
  (* print any references *)
  begin
    match !references with
    | None -> ()
    | Some r ->
      let print_ref (name, (url, title)) =
        if String.equal title "" then Printf.bprintf b "[%s]: %s \n" name url
        else Printf.bprintf b "[%s]: %s \"%s\"\n" name url title
      in
      Buffer.add_char b '\n';
      List.iter ~f:print_ref r#get_all
  end;
  Buffer.contents b

let mrkdwn_of_markdown str = unescape_omd @@ to_markdown @@ transform_list @@ of_string str
