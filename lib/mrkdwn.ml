open Omd

(* https://api.slack.com/reference/surfaces/formatting#escaping *)
let escape_mrkdwn =
  let c_map = function
    | '<' -> "&lt;"
    | '>' -> "&gt;"
    | '&' -> "&amp;"
    | c -> String.make 1 c
  in
  ExtLib.String.fold_left (fun s c -> s ^ c_map c) ""

(** Translates omd AST to a Slack mrkdwn string. Code heavily adapted
    from omd 1.3.1 source.
    https://github.com/ocaml/omd/blob/1.3.1/src/omd_backend.ml#L872
*)
let rec mrkdwn_of_md md =
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
  let rec loop ?(fst_p_in_li = true) ?(is_in_list = false) list_indent =
    (* [list_indent: int] is the indentation level in number of spaces.
       [fst_p_in_li: bool] is used to apply different indentation to the first
       paragraph in a list items.
       [is_in_list: bool] is necessary to know if we are inside a paragraph
       which is inside a list item because those need to be indented!
    *)
    let loop ?(fst_p_in_li = fst_p_in_li) ?(is_in_list = is_in_list) list_indent l =
      loop ~fst_p_in_li ~is_in_list list_indent l
    in
    function
    | [] -> ()
    | el :: tl ->
    match el with
    | X _ -> loop list_indent tl
    | Blockquote q ->
      (* mrkdwn doesn't support nested quotes, but output '>' chars anyway*)
      let quote s =
        let b = Buffer.create (String.length s) in
        let l = String.length s in
        let rec loop is_nl i =
          if i < l then begin
            if is_nl && i < l - 1 then Buffer.add_string b "> ";
            match s.[i] with
            | '\n' ->
              nl b;
              loop true (i + 1)
            | c ->
              Buffer.add_char b c;
              loop false (i + 1)
          end
          else Buffer.contents b
        in
        loop true 0
      in
      Buffer.add_string b (quote @@ mrkdwn_of_md q);
      if tl <> [] then nl_if_needed_above b;
      loop list_indent tl
    | Ref (rc, _name, _text, fallback) | Img_ref (rc, _name, _text, fallback) ->
      (* [rc] stores all refs from document, so it's enough to record just the
         first encounter
      *)
      if Option.is_none !references then references := Some rc;
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
      List.iter (fun li ->
        add_spaces list_indent;
        Buffer.add_string b "- ";
        loop ~is_in_list:true (list_indent + 4) li;
        nl_if_needed_above b
      ) l;
      if list_indent = 0 then nl b;
      loop list_indent tl
    | Ol l ->
      nl_if_needed_above b;
      List.iteri (fun i li ->
        add_spaces list_indent;
        Printf.bprintf b "%d. " (i + 1);
        loop ~is_in_list:true (list_indent + 4) li;
        nl_if_needed_above b
      ) l;
      if list_indent = 0 then nl b;
      loop list_indent tl
    | Ulp l ->
      List.iter (fun li ->
        nl_if_needed_above b;
        add_spaces list_indent;
        Buffer.add_string b "- ";
        loop ~is_in_list:true (list_indent + 4) li (* Paragraphs => No need of '\n' *)
      ) l;
      loop list_indent tl
    | Olp l ->
      List.iteri (fun i li ->
        nl_if_needed_above b;
        add_spaces list_indent;
        Printf.bprintf b "%d. " i;
        loop ~is_in_list:true (list_indent + 4) li (* Paragraphs => No need of '\n' *)
      ) l;
      loop list_indent tl
    | Code_block (_lang, c) ->
      (* unlike commonmark, can't have code block inside lists, so print code block with
         zero indent, but continue rest of the list at correct indent after

         note: sometimes indentation intended as list item paragraph is wrongly
         interpreted as code block - an issue with Omd.of_string
         e.g., both should be parsed the second way, but aren't:
         # of_string "  - foo\n\n    bar";;
         - : t = [Ul [[Text "foo"]]; NL; NL; Code_block ("", "bar")]
         # of_string "- foo\n\n    bar";;
         - : t = [Ulp [[Paragraph [Text "foo"]; Paragraph [Text "bar"]]]]
      *)
      nl_if_needed_above b;
      Buffer.add_string b "```\n";
      Buffer.add_string b (escape_mrkdwn c);
      nl_if_needed_above b;
      Buffer.add_string b "```\n";
      loop list_indent tl
    | Code (_lang, c) ->
      (* sadly, slack mrkdwn has no way to escape backticks within in-line code,
         so broken markup is unavoidable
      *)
      Buffer.add_char b '`';
      Buffer.add_string b (escape_mrkdwn c);
      Buffer.add_char b '`';
      loop list_indent tl
    | Hr ->
      Buffer.add_string b "* * *\n";
      loop list_indent tl
    | Html (_tagname, _attrs, _body) as html -> loop list_indent (Code ("", to_html [ html ]) :: tl)
    | Html_block (_tagname, _attrs, _body) as html -> loop list_indent (Code_block ("", to_html [ html ]) :: tl)
    | Html_comment _s -> loop list_indent tl
    | Url (href, s, title) ->
      Buffer.add_char b '<';
      Buffer.add_string b href;
      Buffer.add_char b '|';
      if String.length title > 0 then Printf.bprintf b "%s - " @@ escape_mrkdwn title;
      loop list_indent s;
      Buffer.add_char b '>';
      loop list_indent tl
    | H1 md' | H2 md' | H3 md' | H4 md' | H5 md' | H6 md' -> loop list_indent (Paragraph [ Bold md' ] :: tl)
    | Br ->
      (* the string "\\n" (backslash-newline) or end of line double-space renders Br *)
      nl b;
      loop list_indent tl
    | NL ->
      (* the string "\n" renders NL *)
      nl_if_needed_above b;
      loop list_indent tl
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
      List.iter print_ref r#get_all
  end;
  Buffer.contents b

let mrkdwn_of_markdown str = String.trim @@ mrkdwn_of_md @@ of_string str
