open Omd
open Devkit

let log = Log.from "mrkdwn"

let htmlentities s =
  let b = Buffer.create (String.length s) in
  let rec loop i =
    if i >= String.length s then Buffer.contents b
    else begin
      begin
        match s.[i] with
        | '"' -> Buffer.add_string b "&quot;"
        | '&' -> Buffer.add_string b "&amp;"
        | '<' -> Buffer.add_string b "&lt;"
        | '>' -> Buffer.add_string b "&gt;"
        | c -> Buffer.add_char b c
      end;
      loop (succ i)
    end
  in
  loop 0

let escape_uri s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | ( '!' | '*' | '\'' | '(' | ')' | ';' | ':' | '@' | '=' | '+' | '$' | ',' | '/' | '?' | '%' | '#'
        | 'A' .. 'Z'
        | 'a' .. 'z'
        | '0' .. '9'
        | '-' | '_' | '.' | '~' ) as c ->
        Buffer.add_char b c
      | '&' -> Buffer.add_string b "&amp;"
      | _ as c -> Printf.bprintf b "%%%2X" (Char.code c))
    s;
  Buffer.contents b

let rec inline { il_desc; il_attributes } =
  let il_desc' =
    match il_desc with
    | Concat l -> Concat (List.map inline l)
    | Html body -> Code body
    | Image x | Link x -> Link { x with destination = escape_uri x.destination }
    | Emph e -> Emph (inline e)
    | Strong s -> Strong (inline s)
    | (Text _ | Code _ | Hard_break | Soft_break) as e -> e
  in
  { il_desc = il_desc'; il_attributes }

let rec block { bl_desc; bl_attributes } =
  let bl_desc' =
    match bl_desc with
    | Blockquote q -> Blockquote (List.map block q)
    | Paragraph md -> Paragraph (inline md)
    | List (ty, sp, bls) -> List (ty, sp, List.map (List.map block) bls)
    | Html_block body -> Code_block ("html", body)
    | Heading (level, text) when level <= 2 -> Heading (level, inline text)
    | Heading (_, text) -> Paragraph { il_desc = Strong (inline text); il_attributes = [] }
    | Definition_list l ->
      let f { term; defs } = { term = inline term; defs = List.map inline defs } in
      Definition_list (List.map f l)
    | (Code_block _ | Thematic_break) as e -> e
  in
  { bl_desc = bl_desc'; bl_attributes }

let quote_render = "> "

let defs_render = ": "

(* inline render arguments *)
type ira = {
  at_start : bool;
  block_quote : bool;
}

let start_ira = { at_start = true; block_quote = false }

let mid_ira = { at_start = false; block_quote = false }

let quote_ira = { at_start = true; block_quote = true }

let rec add_to_buffer_inline buf ?(args = start_ira) { il_desc; _ } =
  let not_start_args' = { args with at_start = false } in
  let start_args' = { args with at_start = true } in
  (* argument aliases *)
  let f = add_to_buffer_inline buf in
  let fc = f ~args:not_start_args' in
  let g = Printf.bprintf buf in
  let g' = Buffer.add_string buf in
  (* recursive call and printing aliases *)
  let insert_block_quote () = if args.at_start && args.block_quote then g' quote_render else () in
  (* helper functions *)
  match il_desc with
  | Concat xs -> List.fold_left (fun args' x -> f x ~args:args') args xs
  | Link { label; destination; title } ->
    insert_block_quote ();
    ( match title with
    | Some v -> g "<%s - " v
    | None -> g' "<"
    );
    let args' = fc label in
    g "|%s>" destination;
    args'
  | Emph e ->
    insert_block_quote ();
    g' "_";
    let args' = fc e in
    g' "_";
    args'
  | Strong s ->
    insert_block_quote ();
    g' "*";
    let args' = fc s in
    g' "*";
    args'
  | Text t ->
    insert_block_quote ();
    g' t;
    not_start_args'
  | Code c ->
    insert_block_quote ();
    g "`%s`" c;
    not_start_args'
  | Hard_break | Soft_break ->
    g' "\n";
    start_args'
  | Html _ ->
    log#error "illegal mrkdwn html inline";
    args
  | Image _ ->
    log#error "illegal mrkdwn image inline";
    args

(* block render arguments *)
type bra = {
  in_quote : bool;
  indent_level : int;
}

let default_bra = { in_quote = false; indent_level = 0 }

let quote_bra = { in_quote = true; indent_level = 0 }

let rec add_to_buffer buf ?(args = default_bra) { bl_desc; _ } =
  let quote_args' = { args with in_quote = true } in
  let inc_indent_args' = { args with indent_level = args.indent_level + 1 } in
  (* argument aliases *)
  let f = add_to_buffer buf in
  let fi = add_to_buffer_inline buf in
  let fim = add_to_buffer_inline buf ~args:mid_ira in
  let fiq = fi ~args:(if args.in_quote then quote_ira else start_ira) in
  let g = Printf.bprintf buf in
  let g' = Buffer.add_string buf in
  (* recursive call and printing aliases *)
  let insert_block_quote () = if args.in_quote then g' quote_render else () in
  match bl_desc with
  | Blockquote qs -> ignore @@ List.map (f ~args:quote_args') qs
  | Paragraph md ->
    ignore @@ fiq md;
    g' "\n"
  | List (ty, _, bls) ->
    let indices = List.init (List.length bls) id in
    let bls_indexed = List.combine indices bls in
    let indent_str = String.make (args.indent_level * 2) ' ' in
    let bullet_str i =
      match ty with
      | Ordered (n, c) -> string_of_int (n + i) ^ Char.escaped c
      | Bullet _ -> "-"
    in
    let f' i ({ bl_desc; _ } as b) =
      match bl_desc with
      | List _ -> f ~args:inc_indent_args' b
      | _ ->
        insert_block_quote ();
        g "%s%s " indent_str @@ bullet_str i;
        f b
    in
    ignore @@ List.map (fun (i, bl) -> List.map (f' i) bl) bls_indexed
  | Heading (n, h) ->
    insert_block_quote ();
    g' @@ String.make n '#';
    g' " ";
    ignore @@ fi ~args:mid_ira h
  | Definition_list l ->
    let fi' d =
      g' defs_render;
      ignore @@ fim d;
      g' "\n"
    in
    let render_term { term; defs } =
      ignore @@ fiq term;
      g' "\n";
      ignore @@ List.map fi' defs;
      g' "\n"
    in
    ignore @@ List.map render_term l
  | Code_block (label, code) -> g "```%s\n%s\n```" label code
  | Thematic_break -> g' "***\n"
  | Html_block _ -> log#error "illegal mrkdwn html block"

let of_doc t = List.map block t

let to_string t =
  let buf = Buffer.create 1024 in
  ignore @@ List.map (fun x -> add_to_buffer buf x) t;
  Buffer.contents buf

let to_mrkdwn doc = to_string @@ of_doc doc
