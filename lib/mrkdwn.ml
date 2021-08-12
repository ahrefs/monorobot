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
  let nl b = Buffer.add_char b '\n' in
  let nl_if_needed_above b =
    if Buffer.length b > 0 && (not @@ Char.equal '\n' (Buffer.nth b (Buffer.length b - 1))) then nl b
  in
  let add_spaces n =
    for _i = 1 to n do
      Buffer.add_char b ' '
    done
  in
  let rec f ?(fst_p_in_li = true) ?(is_in_list = false) list_indent tl = function
    (* [list_indent: int] is the indentation level in number of spaces.
       [fst_p_in_li: bool] is used to apply different indentation to the first
       paragraph in a list items.
       [is_in_list: bool] is necessary to know if we are inside a paragraph
       which is inside a list item because those need to be indented!
    *)
    | X _ -> loop list_indent tl
    | Blockquote _q -> (* todo *) loop list_indent tl
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
      loop list_indent (Raw fallback#to_string :: tl)
    | Paragraph [] -> loop list_indent tl
    | Paragraph md ->
      (* indent if inside a list (Olp or Ulp) *)
      if is_in_list then if fst_p_in_li then add_spaces (list_indent - 2) else add_spaces list_indent;
      (* paragraph body + skip line *)
      loop ~fst_p_in_li:false list_indent md;
      nl b;
      nl b;
      loop ~fst_p_in_li:false list_indent tl
    | Img (alt, src, title) -> loop list_indent (Url (src, [ Text alt ], title) :: tl)
    | Text t ->
      Buffer.add_string b @@ escape_mrkdwn t;
      loop list_indent tl
    | Raw s ->
      Buffer.add_string b s;
      loop list_indent tl
    | Raw_block s ->
      nl b;
      Buffer.add_string b s;
      nl b;
      loop list_indent tl
    | Emph md' ->
      Buffer.add_string b "_";
      loop list_indent md';
      Buffer.add_string b "_";
      loop list_indent tl
    | Bold md' ->
      Buffer.add_string b "*";
      loop list_indent md';
      Buffer.add_string b "*";
      loop list_indent tl
    | Ul l ->
      nl_if_needed_above b;
      List.iter l ~f:(fun li ->
        add_spaces list_indent;
        Buffer.add_string b "- ";
        loop ~is_in_list:true (list_indent + 4) li;
        nl_if_needed_above b);
      if list_indent = 0 then nl b;
      loop list_indent tl
    | Ol l ->
      nl_if_needed_above b;
      List.iteri l ~f:(fun i li ->
        add_spaces list_indent;
        Printf.bprintf b "%d. " (i + 1);
        loop ~is_in_list:true (list_indent + 4) li;
        nl_if_needed_above b);
      if list_indent = 0 then nl b;
      loop list_indent tl
    | Ulp l ->
      List.iter l ~f:(fun li ->
        nl_if_needed_above b;
        add_spaces list_indent;
        Buffer.add_string b "- ";
        loop ~is_in_list:true (list_indent + 4) li (* Paragraphs => No need of '\n' *));
      loop list_indent tl
    | Olp l ->
      List.iteri l ~f:(fun i li ->
        nl_if_needed_above b;
        add_spaces list_indent;
        Printf.bprintf b "%d. " i;
        loop ~is_in_list:true (list_indent + 4) li (* Paragraphs => No need of '\n' *));
      loop list_indent tl
    | Code_block (_lang, _c) -> (* todo *) loop list_indent tl
    | Code (_lang, c) ->
      Buffer.add_char b '`';
      Buffer.add_string b (escape_mrkdwn c);
      Buffer.add_char b '`';
      loop list_indent tl
    | Hr ->
      Buffer.add_string b "* * *\n";
      loop list_indent tl
    | Html (tagname, _attrs, body) ->
      Printf.bprintf b "`&lt;%s&gt;" tagname;
      Buffer.add_string b (escape_mrkdwn @@ to_html body);
      Printf.bprintf b "&lt;/%s&gt;`" tagname;
      loop list_indent tl
    | Html_block (_tagname, _attrs, _body) -> (* todo *) loop list_indent tl
    | Html_comment _s -> loop list_indent tl
    | Url (href, s, title) ->
      Buffer.add_char b '<';
      Buffer.add_string b href;
      Buffer.add_char b '|';
      if String.length title > 0 then Printf.bprintf b "%s - " @@ escape_mrkdwn title;
      loop list_indent s;
      Buffer.add_char b '>';
      loop list_indent tl
    | H1 md' | H2 md' | H3 md' | H4 md' | H5 md' | H6 md' ->
      loop list_indent (Paragraph (transform_list [ Bold md' ]) :: tl)
    | Br ->
      (* the string "\\n" (backslash-newline) or end of line double-space renders Br *)
      nl b;
      loop list_indent tl
    | NL ->
      (* the string "\n" renders NL *)
      nl_if_needed_above b;
      loop list_indent tl
  and loop ?(fst_p_in_li = true) ?(is_in_list = false) list_indent = function
    | hd :: tl -> f ~fst_p_in_li ~is_in_list list_indent tl hd
    | [] -> ()
  in
  (* print the document *)
  loop 0 md;
  (* print any references *)
  begin
    match !references with
    | None -> ()
    | Some r ->
      let print_ref (name, (url, title)) =
        if String.equal title "" then Printf.bprintf b "[%s]: %s \n" name url
        else Printf.bprintf b "[%s]: %s \"%s\"\n" name url title
      in
      nl b;
      List.iter ~f:print_ref r#get_all
  end;
  Buffer.contents b

let mrkdwn_of_markdown str = unescape_omd @@ to_markdown @@ transform_list @@ of_string str
