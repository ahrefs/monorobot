(* Auto-generated from "events_notifications.atd" *)
[@@@ocaml.warning "-27-32-35-39"]

type user = Events_notifications_t.user = {
  user_login (*atd login *) : string;
  user_id (*atd id *) : int;
}

type pull_request = Events_notifications_t.pull_request = {
  pull_request_id (*atd id *) : int;
  pull_request_body (*atd body *) : string;
  pull_request_title (*atd title *) : string;
  pull_request_url (*atd url *) : string;
}

type pr_notification = Events_notifications_t.pr_notification = {
  action : string;
  number : int;
  sender : user;
  pull_request : pull_request;
}

type author = Events_notifications_t.author = {
  name : string;
  email : string;
  username : string option;
}

type commit = Events_notifications_t.commit = {
  commit_id (*atd id *) : string;
  commit_message (*atd message *) : string;
  commit_timestamp (*atd timestamp *) : string;
  commit_url (*atd url *) : string;
  commit_author (*atd author *) : author;
  commit_committer (*atd committer *) : author;
}

type commit_pushed_notification = Events_notifications_t.commit_pushed_notification = {
  ref : string;
  after : string;
  commits : commit list;
  head_commit : commit;
  pusher : author;
  sender : user;
}

type branch = Events_notifications_t.branch = { name : string }

type ci_build_notification = Events_notifications_t.ci_build_notification = {
  commit : commit;
  state : string;
  target_url : string;
  branches : branch list;
}

let write_user : _ -> user -> _ =
 fun ob x ->
  Bi_outbuf.add_char ob '{';
  let is_first = ref true in
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"login\":";
  Yojson.Safe.write_string ob x.user_login;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"id\":";
  Yojson.Safe.write_int ob x.user_id;
  Bi_outbuf.add_char ob '}'

let string_of_user ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_user ob x;
  Bi_outbuf.contents ob

let read_user p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_lcurl p lb;
  let field_user_login = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_user_id = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let bits0 = ref 0 in
  try
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_object_end lb;
    Yojson.Safe.read_space p lb;
    let f s pos len =
      if pos < 0 || len < 0 || pos + len > String.length s then
        invalid_arg "out-of-bounds substring position or length";
      match len with
      | 2 -> if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos + 1) = 'd' then 1 else -1
      | 5 ->
        if
          String.unsafe_get s pos = 'l'
          && String.unsafe_get s (pos + 1) = 'o'
          && String.unsafe_get s (pos + 2) = 'g'
          && String.unsafe_get s (pos + 3) = 'i'
          && String.unsafe_get s (pos + 4) = 'n'
        then 0
        else -1
      | _ -> -1
    in
    let i = Yojson.Safe.map_ident p f lb in
    Atdgen_runtime.Oj_run.read_until_field_value p lb;
    ( match i with
    | 0 ->
      field_user_login := Atdgen_runtime.Oj_run.read_string p lb;
      bits0 := !bits0 lor 0x1
    | 1 ->
      field_user_id := Atdgen_runtime.Oj_run.read_int p lb;
      bits0 := !bits0 lor 0x2
    | _ -> Yojson.Safe.skip_json p lb );
    while true do
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_sep p lb;
      Yojson.Safe.read_space p lb;
      let f s pos len =
        if pos < 0 || len < 0 || pos + len > String.length s then
          invalid_arg "out-of-bounds substring position or length";
        match len with
        | 2 -> if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos + 1) = 'd' then 1 else -1
        | 5 ->
          if
            String.unsafe_get s pos = 'l'
            && String.unsafe_get s (pos + 1) = 'o'
            && String.unsafe_get s (pos + 2) = 'g'
            && String.unsafe_get s (pos + 3) = 'i'
            && String.unsafe_get s (pos + 4) = 'n'
          then 0
          else -1
        | _ -> -1
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      match i with
      | 0 ->
        field_user_login := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x1
      | 1 ->
        field_user_id := Atdgen_runtime.Oj_run.read_int p lb;
        bits0 := !bits0 lor 0x2
      | _ -> Yojson.Safe.skip_json p lb
    done;
    assert false
  with Yojson.End_of_object ->
    if !bits0 <> 0x3 then Atdgen_runtime.Oj_run.missing_fields p [| !bits0 |] [| "login"; "id" |];
    ({ user_login = !field_user_login; user_id = !field_user_id } : user)

let user_of_string s = read_user (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write_pull_request : _ -> pull_request -> _ =
 fun ob x ->
  Bi_outbuf.add_char ob '{';
  let is_first = ref true in
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"id\":";
  Yojson.Safe.write_int ob x.pull_request_id;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"body\":";
  Yojson.Safe.write_string ob x.pull_request_body;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"title\":";
  Yojson.Safe.write_string ob x.pull_request_title;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"url\":";
  Yojson.Safe.write_string ob x.pull_request_url;
  Bi_outbuf.add_char ob '}'

let string_of_pull_request ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_pull_request ob x;
  Bi_outbuf.contents ob

let read_pull_request p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_lcurl p lb;
  let field_pull_request_id = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_pull_request_body = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_pull_request_title = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_pull_request_url = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let bits0 = ref 0 in
  try
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_object_end lb;
    Yojson.Safe.read_space p lb;
    let f s pos len =
      if pos < 0 || len < 0 || pos + len > String.length s then
        invalid_arg "out-of-bounds substring position or length";
      match len with
      | 2 -> if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos + 1) = 'd' then 0 else -1
      | 3 ->
        if String.unsafe_get s pos = 'u' && String.unsafe_get s (pos + 1) = 'r' && String.unsafe_get s (pos + 2) = 'l'
        then 3
        else -1
      | 4 ->
        if
          String.unsafe_get s pos = 'b'
          && String.unsafe_get s (pos + 1) = 'o'
          && String.unsafe_get s (pos + 2) = 'd'
          && String.unsafe_get s (pos + 3) = 'y'
        then 1
        else -1
      | 5 ->
        if
          String.unsafe_get s pos = 't'
          && String.unsafe_get s (pos + 1) = 'i'
          && String.unsafe_get s (pos + 2) = 't'
          && String.unsafe_get s (pos + 3) = 'l'
          && String.unsafe_get s (pos + 4) = 'e'
        then 2
        else -1
      | _ -> -1
    in
    let i = Yojson.Safe.map_ident p f lb in
    Atdgen_runtime.Oj_run.read_until_field_value p lb;
    ( match i with
    | 0 ->
      field_pull_request_id := Atdgen_runtime.Oj_run.read_int p lb;
      bits0 := !bits0 lor 0x1
    | 1 ->
      field_pull_request_body := Atdgen_runtime.Oj_run.read_string p lb;
      bits0 := !bits0 lor 0x2
    | 2 ->
      field_pull_request_title := Atdgen_runtime.Oj_run.read_string p lb;
      bits0 := !bits0 lor 0x4
    | 3 ->
      field_pull_request_url := Atdgen_runtime.Oj_run.read_string p lb;
      bits0 := !bits0 lor 0x8
    | _ -> Yojson.Safe.skip_json p lb );
    while true do
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_sep p lb;
      Yojson.Safe.read_space p lb;
      let f s pos len =
        if pos < 0 || len < 0 || pos + len > String.length s then
          invalid_arg "out-of-bounds substring position or length";
        match len with
        | 2 -> if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos + 1) = 'd' then 0 else -1
        | 3 ->
          if
            String.unsafe_get s pos = 'u' && String.unsafe_get s (pos + 1) = 'r' && String.unsafe_get s (pos + 2) = 'l'
          then 3
          else -1
        | 4 ->
          if
            String.unsafe_get s pos = 'b'
            && String.unsafe_get s (pos + 1) = 'o'
            && String.unsafe_get s (pos + 2) = 'd'
            && String.unsafe_get s (pos + 3) = 'y'
          then 1
          else -1
        | 5 ->
          if
            String.unsafe_get s pos = 't'
            && String.unsafe_get s (pos + 1) = 'i'
            && String.unsafe_get s (pos + 2) = 't'
            && String.unsafe_get s (pos + 3) = 'l'
            && String.unsafe_get s (pos + 4) = 'e'
          then 2
          else -1
        | _ -> -1
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      match i with
      | 0 ->
        field_pull_request_id := Atdgen_runtime.Oj_run.read_int p lb;
        bits0 := !bits0 lor 0x1
      | 1 ->
        field_pull_request_body := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x2
      | 2 ->
        field_pull_request_title := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x4
      | 3 ->
        field_pull_request_url := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x8
      | _ -> Yojson.Safe.skip_json p lb
    done;
    assert false
  with Yojson.End_of_object ->
    if !bits0 <> 0xf then Atdgen_runtime.Oj_run.missing_fields p [| !bits0 |] [| "id"; "body"; "title"; "url" |];
    ( {
        pull_request_id = !field_pull_request_id;
        pull_request_body = !field_pull_request_body;
        pull_request_title = !field_pull_request_title;
        pull_request_url = !field_pull_request_url;
      }
      : pull_request )

let pull_request_of_string s = read_pull_request (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write_pr_notification : _ -> pr_notification -> _ =
 fun ob x ->
  Bi_outbuf.add_char ob '{';
  let is_first = ref true in
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"action\":";
  Yojson.Safe.write_string ob x.action;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"number\":";
  Yojson.Safe.write_int ob x.number;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"sender\":";
  write_user ob x.sender;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"pull_request\":";
  write_pull_request ob x.pull_request;
  Bi_outbuf.add_char ob '}'

let string_of_pr_notification ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_pr_notification ob x;
  Bi_outbuf.contents ob

let read_pr_notification p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_lcurl p lb;
  let field_action = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_number = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_sender = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_pull_request = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let bits0 = ref 0 in
  try
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_object_end lb;
    Yojson.Safe.read_space p lb;
    let f s pos len =
      if pos < 0 || len < 0 || pos + len > String.length s then
        invalid_arg "out-of-bounds substring position or length";
      match len with
      | 6 ->
        ( match String.unsafe_get s pos with
        | 'a' ->
          if
            String.unsafe_get s (pos + 1) = 'c'
            && String.unsafe_get s (pos + 2) = 't'
            && String.unsafe_get s (pos + 3) = 'i'
            && String.unsafe_get s (pos + 4) = 'o'
            && String.unsafe_get s (pos + 5) = 'n'
          then 0
          else -1
        | 'n' ->
          if
            String.unsafe_get s (pos + 1) = 'u'
            && String.unsafe_get s (pos + 2) = 'm'
            && String.unsafe_get s (pos + 3) = 'b'
            && String.unsafe_get s (pos + 4) = 'e'
            && String.unsafe_get s (pos + 5) = 'r'
          then 1
          else -1
        | 's' ->
          if
            String.unsafe_get s (pos + 1) = 'e'
            && String.unsafe_get s (pos + 2) = 'n'
            && String.unsafe_get s (pos + 3) = 'd'
            && String.unsafe_get s (pos + 4) = 'e'
            && String.unsafe_get s (pos + 5) = 'r'
          then 2
          else -1
        | _ -> -1 )
      | 12 ->
        if
          String.unsafe_get s pos = 'p'
          && String.unsafe_get s (pos + 1) = 'u'
          && String.unsafe_get s (pos + 2) = 'l'
          && String.unsafe_get s (pos + 3) = 'l'
          && String.unsafe_get s (pos + 4) = '_'
          && String.unsafe_get s (pos + 5) = 'r'
          && String.unsafe_get s (pos + 6) = 'e'
          && String.unsafe_get s (pos + 7) = 'q'
          && String.unsafe_get s (pos + 8) = 'u'
          && String.unsafe_get s (pos + 9) = 'e'
          && String.unsafe_get s (pos + 10) = 's'
          && String.unsafe_get s (pos + 11) = 't'
        then 3
        else -1
      | _ -> -1
    in
    let i = Yojson.Safe.map_ident p f lb in
    Atdgen_runtime.Oj_run.read_until_field_value p lb;
    ( match i with
    | 0 ->
      field_action := Atdgen_runtime.Oj_run.read_string p lb;
      bits0 := !bits0 lor 0x1
    | 1 ->
      field_number := Atdgen_runtime.Oj_run.read_int p lb;
      bits0 := !bits0 lor 0x2
    | 2 ->
      field_sender := read_user p lb;
      bits0 := !bits0 lor 0x4
    | 3 ->
      field_pull_request := read_pull_request p lb;
      bits0 := !bits0 lor 0x8
    | _ -> Yojson.Safe.skip_json p lb );
    while true do
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_sep p lb;
      Yojson.Safe.read_space p lb;
      let f s pos len =
        if pos < 0 || len < 0 || pos + len > String.length s then
          invalid_arg "out-of-bounds substring position or length";
        match len with
        | 6 ->
          ( match String.unsafe_get s pos with
          | 'a' ->
            if
              String.unsafe_get s (pos + 1) = 'c'
              && String.unsafe_get s (pos + 2) = 't'
              && String.unsafe_get s (pos + 3) = 'i'
              && String.unsafe_get s (pos + 4) = 'o'
              && String.unsafe_get s (pos + 5) = 'n'
            then 0
            else -1
          | 'n' ->
            if
              String.unsafe_get s (pos + 1) = 'u'
              && String.unsafe_get s (pos + 2) = 'm'
              && String.unsafe_get s (pos + 3) = 'b'
              && String.unsafe_get s (pos + 4) = 'e'
              && String.unsafe_get s (pos + 5) = 'r'
            then 1
            else -1
          | 's' ->
            if
              String.unsafe_get s (pos + 1) = 'e'
              && String.unsafe_get s (pos + 2) = 'n'
              && String.unsafe_get s (pos + 3) = 'd'
              && String.unsafe_get s (pos + 4) = 'e'
              && String.unsafe_get s (pos + 5) = 'r'
            then 2
            else -1
          | _ -> -1 )
        | 12 ->
          if
            String.unsafe_get s pos = 'p'
            && String.unsafe_get s (pos + 1) = 'u'
            && String.unsafe_get s (pos + 2) = 'l'
            && String.unsafe_get s (pos + 3) = 'l'
            && String.unsafe_get s (pos + 4) = '_'
            && String.unsafe_get s (pos + 5) = 'r'
            && String.unsafe_get s (pos + 6) = 'e'
            && String.unsafe_get s (pos + 7) = 'q'
            && String.unsafe_get s (pos + 8) = 'u'
            && String.unsafe_get s (pos + 9) = 'e'
            && String.unsafe_get s (pos + 10) = 's'
            && String.unsafe_get s (pos + 11) = 't'
          then 3
          else -1
        | _ -> -1
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      match i with
      | 0 ->
        field_action := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x1
      | 1 ->
        field_number := Atdgen_runtime.Oj_run.read_int p lb;
        bits0 := !bits0 lor 0x2
      | 2 ->
        field_sender := read_user p lb;
        bits0 := !bits0 lor 0x4
      | 3 ->
        field_pull_request := read_pull_request p lb;
        bits0 := !bits0 lor 0x8
      | _ -> Yojson.Safe.skip_json p lb
    done;
    assert false
  with Yojson.End_of_object ->
    if !bits0 <> 0xf then
      Atdgen_runtime.Oj_run.missing_fields p [| !bits0 |] [| "action"; "number"; "sender"; "pull_request" |];
    ( { action = !field_action; number = !field_number; sender = !field_sender; pull_request = !field_pull_request }
      : pr_notification )

let pr_notification_of_string s = read_pr_notification (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write__1 = Atdgen_runtime.Oj_run.write_option Yojson.Safe.write_string

let string_of__1 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__1 ob x;
  Bi_outbuf.contents ob

let read__1 p lb =
  Yojson.Safe.read_space p lb;
  match Yojson.Safe.start_any_variant p lb with
  | `Edgy_bracket ->
    ( match Yojson.Safe.read_ident p lb with
    | "None" ->
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_gt p lb;
      (None : _ option)
    | "Some" ->
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      let x = Atdgen_runtime.Oj_run.read_string p lb in
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_gt p lb;
      (Some x : _ option)
    | x -> Atdgen_runtime.Oj_run.invalid_variant_tag p x )
  | `Double_quote ->
    ( match Yojson.Safe.finish_string p lb with
    | "None" -> (None : _ option)
    | x -> Atdgen_runtime.Oj_run.invalid_variant_tag p x )
  | `Square_bracket ->
    ( match Atdgen_runtime.Oj_run.read_string p lb with
    | "Some" ->
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_comma p lb;
      Yojson.Safe.read_space p lb;
      let x = Atdgen_runtime.Oj_run.read_string p lb in
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_rbr p lb;
      (Some x : _ option)
    | x -> Atdgen_runtime.Oj_run.invalid_variant_tag p x )

let _1_of_string s = read__1 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write_author : _ -> author -> _ =
 fun ob x ->
  Bi_outbuf.add_char ob '{';
  let is_first = ref true in
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"name\":";
  Yojson.Safe.write_string ob x.name;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"email\":";
  Yojson.Safe.write_string ob x.email;
  ( match x.username with
  | None -> ()
  | Some x ->
    if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"username\":";
    Yojson.Safe.write_string ob x );
  Bi_outbuf.add_char ob '}'

let string_of_author ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_author ob x;
  Bi_outbuf.contents ob

let read_author p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_lcurl p lb;
  let field_name = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_email = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_username = ref None in
  let bits0 = ref 0 in
  try
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_object_end lb;
    Yojson.Safe.read_space p lb;
    let f s pos len =
      if pos < 0 || len < 0 || pos + len > String.length s then
        invalid_arg "out-of-bounds substring position or length";
      match len with
      | 4 ->
        if
          String.unsafe_get s pos = 'n'
          && String.unsafe_get s (pos + 1) = 'a'
          && String.unsafe_get s (pos + 2) = 'm'
          && String.unsafe_get s (pos + 3) = 'e'
        then 0
        else -1
      | 5 ->
        if
          String.unsafe_get s pos = 'e'
          && String.unsafe_get s (pos + 1) = 'm'
          && String.unsafe_get s (pos + 2) = 'a'
          && String.unsafe_get s (pos + 3) = 'i'
          && String.unsafe_get s (pos + 4) = 'l'
        then 1
        else -1
      | 8 ->
        if
          String.unsafe_get s pos = 'u'
          && String.unsafe_get s (pos + 1) = 's'
          && String.unsafe_get s (pos + 2) = 'e'
          && String.unsafe_get s (pos + 3) = 'r'
          && String.unsafe_get s (pos + 4) = 'n'
          && String.unsafe_get s (pos + 5) = 'a'
          && String.unsafe_get s (pos + 6) = 'm'
          && String.unsafe_get s (pos + 7) = 'e'
        then 2
        else -1
      | _ -> -1
    in
    let i = Yojson.Safe.map_ident p f lb in
    Atdgen_runtime.Oj_run.read_until_field_value p lb;
    ( match i with
    | 0 ->
      field_name := Atdgen_runtime.Oj_run.read_string p lb;
      bits0 := !bits0 lor 0x1
    | 1 ->
      field_email := Atdgen_runtime.Oj_run.read_string p lb;
      bits0 := !bits0 lor 0x2
    | 2 ->
      if not (Yojson.Safe.read_null_if_possible p lb) then
        field_username := Some (Atdgen_runtime.Oj_run.read_string p lb)
    | _ -> Yojson.Safe.skip_json p lb );
    while true do
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_sep p lb;
      Yojson.Safe.read_space p lb;
      let f s pos len =
        if pos < 0 || len < 0 || pos + len > String.length s then
          invalid_arg "out-of-bounds substring position or length";
        match len with
        | 4 ->
          if
            String.unsafe_get s pos = 'n'
            && String.unsafe_get s (pos + 1) = 'a'
            && String.unsafe_get s (pos + 2) = 'm'
            && String.unsafe_get s (pos + 3) = 'e'
          then 0
          else -1
        | 5 ->
          if
            String.unsafe_get s pos = 'e'
            && String.unsafe_get s (pos + 1) = 'm'
            && String.unsafe_get s (pos + 2) = 'a'
            && String.unsafe_get s (pos + 3) = 'i'
            && String.unsafe_get s (pos + 4) = 'l'
          then 1
          else -1
        | 8 ->
          if
            String.unsafe_get s pos = 'u'
            && String.unsafe_get s (pos + 1) = 's'
            && String.unsafe_get s (pos + 2) = 'e'
            && String.unsafe_get s (pos + 3) = 'r'
            && String.unsafe_get s (pos + 4) = 'n'
            && String.unsafe_get s (pos + 5) = 'a'
            && String.unsafe_get s (pos + 6) = 'm'
            && String.unsafe_get s (pos + 7) = 'e'
          then 2
          else -1
        | _ -> -1
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      match i with
      | 0 ->
        field_name := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x1
      | 1 ->
        field_email := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x2
      | 2 ->
        if not (Yojson.Safe.read_null_if_possible p lb) then
          field_username := Some (Atdgen_runtime.Oj_run.read_string p lb)
      | _ -> Yojson.Safe.skip_json p lb
    done;
    assert false
  with Yojson.End_of_object ->
    if !bits0 <> 0x3 then Atdgen_runtime.Oj_run.missing_fields p [| !bits0 |] [| "name"; "email" |];
    ({ name = !field_name; email = !field_email; username = !field_username } : author)

let author_of_string s = read_author (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write_commit : _ -> commit -> _ =
 fun ob x ->
  Bi_outbuf.add_char ob '{';
  let is_first = ref true in
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"id\":";
  Yojson.Safe.write_string ob x.commit_id;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"message\":";
  Yojson.Safe.write_string ob x.commit_message;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"timestamp\":";
  Yojson.Safe.write_string ob x.commit_timestamp;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"url\":";
  Yojson.Safe.write_string ob x.commit_url;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"author\":";
  write_author ob x.commit_author;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"committer\":";
  write_author ob x.commit_committer;
  Bi_outbuf.add_char ob '}'

let string_of_commit ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_commit ob x;
  Bi_outbuf.contents ob

let read_commit p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_lcurl p lb;
  let field_commit_id = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_commit_message = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_commit_timestamp = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_commit_url = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_commit_author = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_commit_committer = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let bits0 = ref 0 in
  try
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_object_end lb;
    Yojson.Safe.read_space p lb;
    let f s pos len =
      if pos < 0 || len < 0 || pos + len > String.length s then
        invalid_arg "out-of-bounds substring position or length";
      match len with
      | 2 -> if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos + 1) = 'd' then 0 else -1
      | 3 ->
        if String.unsafe_get s pos = 'u' && String.unsafe_get s (pos + 1) = 'r' && String.unsafe_get s (pos + 2) = 'l'
        then 3
        else -1
      | 6 ->
        if
          String.unsafe_get s pos = 'a'
          && String.unsafe_get s (pos + 1) = 'u'
          && String.unsafe_get s (pos + 2) = 't'
          && String.unsafe_get s (pos + 3) = 'h'
          && String.unsafe_get s (pos + 4) = 'o'
          && String.unsafe_get s (pos + 5) = 'r'
        then 4
        else -1
      | 7 ->
        if
          String.unsafe_get s pos = 'm'
          && String.unsafe_get s (pos + 1) = 'e'
          && String.unsafe_get s (pos + 2) = 's'
          && String.unsafe_get s (pos + 3) = 's'
          && String.unsafe_get s (pos + 4) = 'a'
          && String.unsafe_get s (pos + 5) = 'g'
          && String.unsafe_get s (pos + 6) = 'e'
        then 1
        else -1
      | 9 ->
        ( match String.unsafe_get s pos with
        | 'c' ->
          if
            String.unsafe_get s (pos + 1) = 'o'
            && String.unsafe_get s (pos + 2) = 'm'
            && String.unsafe_get s (pos + 3) = 'm'
            && String.unsafe_get s (pos + 4) = 'i'
            && String.unsafe_get s (pos + 5) = 't'
            && String.unsafe_get s (pos + 6) = 't'
            && String.unsafe_get s (pos + 7) = 'e'
            && String.unsafe_get s (pos + 8) = 'r'
          then 5
          else -1
        | 't' ->
          if
            String.unsafe_get s (pos + 1) = 'i'
            && String.unsafe_get s (pos + 2) = 'm'
            && String.unsafe_get s (pos + 3) = 'e'
            && String.unsafe_get s (pos + 4) = 's'
            && String.unsafe_get s (pos + 5) = 't'
            && String.unsafe_get s (pos + 6) = 'a'
            && String.unsafe_get s (pos + 7) = 'm'
            && String.unsafe_get s (pos + 8) = 'p'
          then 2
          else -1
        | _ -> -1 )
      | _ -> -1
    in
    let i = Yojson.Safe.map_ident p f lb in
    Atdgen_runtime.Oj_run.read_until_field_value p lb;
    ( match i with
    | 0 ->
      field_commit_id := Atdgen_runtime.Oj_run.read_string p lb;
      bits0 := !bits0 lor 0x1
    | 1 ->
      field_commit_message := Atdgen_runtime.Oj_run.read_string p lb;
      bits0 := !bits0 lor 0x2
    | 2 ->
      field_commit_timestamp := Atdgen_runtime.Oj_run.read_string p lb;
      bits0 := !bits0 lor 0x4
    | 3 ->
      field_commit_url := Atdgen_runtime.Oj_run.read_string p lb;
      bits0 := !bits0 lor 0x8
    | 4 ->
      field_commit_author := read_author p lb;
      bits0 := !bits0 lor 0x10
    | 5 ->
      field_commit_committer := read_author p lb;
      bits0 := !bits0 lor 0x20
    | _ -> Yojson.Safe.skip_json p lb );
    while true do
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_sep p lb;
      Yojson.Safe.read_space p lb;
      let f s pos len =
        if pos < 0 || len < 0 || pos + len > String.length s then
          invalid_arg "out-of-bounds substring position or length";
        match len with
        | 2 -> if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos + 1) = 'd' then 0 else -1
        | 3 ->
          if
            String.unsafe_get s pos = 'u' && String.unsafe_get s (pos + 1) = 'r' && String.unsafe_get s (pos + 2) = 'l'
          then 3
          else -1
        | 6 ->
          if
            String.unsafe_get s pos = 'a'
            && String.unsafe_get s (pos + 1) = 'u'
            && String.unsafe_get s (pos + 2) = 't'
            && String.unsafe_get s (pos + 3) = 'h'
            && String.unsafe_get s (pos + 4) = 'o'
            && String.unsafe_get s (pos + 5) = 'r'
          then 4
          else -1
        | 7 ->
          if
            String.unsafe_get s pos = 'm'
            && String.unsafe_get s (pos + 1) = 'e'
            && String.unsafe_get s (pos + 2) = 's'
            && String.unsafe_get s (pos + 3) = 's'
            && String.unsafe_get s (pos + 4) = 'a'
            && String.unsafe_get s (pos + 5) = 'g'
            && String.unsafe_get s (pos + 6) = 'e'
          then 1
          else -1
        | 9 ->
          ( match String.unsafe_get s pos with
          | 'c' ->
            if
              String.unsafe_get s (pos + 1) = 'o'
              && String.unsafe_get s (pos + 2) = 'm'
              && String.unsafe_get s (pos + 3) = 'm'
              && String.unsafe_get s (pos + 4) = 'i'
              && String.unsafe_get s (pos + 5) = 't'
              && String.unsafe_get s (pos + 6) = 't'
              && String.unsafe_get s (pos + 7) = 'e'
              && String.unsafe_get s (pos + 8) = 'r'
            then 5
            else -1
          | 't' ->
            if
              String.unsafe_get s (pos + 1) = 'i'
              && String.unsafe_get s (pos + 2) = 'm'
              && String.unsafe_get s (pos + 3) = 'e'
              && String.unsafe_get s (pos + 4) = 's'
              && String.unsafe_get s (pos + 5) = 't'
              && String.unsafe_get s (pos + 6) = 'a'
              && String.unsafe_get s (pos + 7) = 'm'
              && String.unsafe_get s (pos + 8) = 'p'
            then 2
            else -1
          | _ -> -1 )
        | _ -> -1
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      match i with
      | 0 ->
        field_commit_id := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x1
      | 1 ->
        field_commit_message := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x2
      | 2 ->
        field_commit_timestamp := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x4
      | 3 ->
        field_commit_url := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x8
      | 4 ->
        field_commit_author := read_author p lb;
        bits0 := !bits0 lor 0x10
      | 5 ->
        field_commit_committer := read_author p lb;
        bits0 := !bits0 lor 0x20
      | _ -> Yojson.Safe.skip_json p lb
    done;
    assert false
  with Yojson.End_of_object ->
    if !bits0 <> 0x3f then
      Atdgen_runtime.Oj_run.missing_fields p [| !bits0 |]
        [| "id"; "message"; "timestamp"; "url"; "author"; "committer" |];
    ( {
        commit_id = !field_commit_id;
        commit_message = !field_commit_message;
        commit_timestamp = !field_commit_timestamp;
        commit_url = !field_commit_url;
        commit_author = !field_commit_author;
        commit_committer = !field_commit_committer;
      }
      : commit )

let commit_of_string s = read_commit (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write__2 = Atdgen_runtime.Oj_run.write_list write_commit

let string_of__2 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__2 ob x;
  Bi_outbuf.contents ob

let read__2 = Atdgen_runtime.Oj_run.read_list read_commit

let _2_of_string s = read__2 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write_commit_pushed_notification : _ -> commit_pushed_notification -> _ =
 fun ob x ->
  Bi_outbuf.add_char ob '{';
  let is_first = ref true in
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"ref\":";
  Yojson.Safe.write_string ob x.ref;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"after\":";
  Yojson.Safe.write_string ob x.after;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"commits\":";
  write__2 ob x.commits;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"head_commit\":";
  write_commit ob x.head_commit;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"pusher\":";
  write_author ob x.pusher;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"sender\":";
  write_user ob x.sender;
  Bi_outbuf.add_char ob '}'

let string_of_commit_pushed_notification ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_commit_pushed_notification ob x;
  Bi_outbuf.contents ob

let read_commit_pushed_notification p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_lcurl p lb;
  let field_ref = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_after = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_commits = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_head_commit = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_pusher = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_sender = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let bits0 = ref 0 in
  try
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_object_end lb;
    Yojson.Safe.read_space p lb;
    let f s pos len =
      if pos < 0 || len < 0 || pos + len > String.length s then
        invalid_arg "out-of-bounds substring position or length";
      match len with
      | 3 ->
        if String.unsafe_get s pos = 'r' && String.unsafe_get s (pos + 1) = 'e' && String.unsafe_get s (pos + 2) = 'f'
        then 0
        else -1
      | 5 ->
        if
          String.unsafe_get s pos = 'a'
          && String.unsafe_get s (pos + 1) = 'f'
          && String.unsafe_get s (pos + 2) = 't'
          && String.unsafe_get s (pos + 3) = 'e'
          && String.unsafe_get s (pos + 4) = 'r'
        then 1
        else -1
      | 6 ->
        ( match String.unsafe_get s pos with
        | 'p' ->
          if
            String.unsafe_get s (pos + 1) = 'u'
            && String.unsafe_get s (pos + 2) = 's'
            && String.unsafe_get s (pos + 3) = 'h'
            && String.unsafe_get s (pos + 4) = 'e'
            && String.unsafe_get s (pos + 5) = 'r'
          then 4
          else -1
        | 's' ->
          if
            String.unsafe_get s (pos + 1) = 'e'
            && String.unsafe_get s (pos + 2) = 'n'
            && String.unsafe_get s (pos + 3) = 'd'
            && String.unsafe_get s (pos + 4) = 'e'
            && String.unsafe_get s (pos + 5) = 'r'
          then 5
          else -1
        | _ -> -1 )
      | 7 ->
        if
          String.unsafe_get s pos = 'c'
          && String.unsafe_get s (pos + 1) = 'o'
          && String.unsafe_get s (pos + 2) = 'm'
          && String.unsafe_get s (pos + 3) = 'm'
          && String.unsafe_get s (pos + 4) = 'i'
          && String.unsafe_get s (pos + 5) = 't'
          && String.unsafe_get s (pos + 6) = 's'
        then 2
        else -1
      | 11 ->
        if
          String.unsafe_get s pos = 'h'
          && String.unsafe_get s (pos + 1) = 'e'
          && String.unsafe_get s (pos + 2) = 'a'
          && String.unsafe_get s (pos + 3) = 'd'
          && String.unsafe_get s (pos + 4) = '_'
          && String.unsafe_get s (pos + 5) = 'c'
          && String.unsafe_get s (pos + 6) = 'o'
          && String.unsafe_get s (pos + 7) = 'm'
          && String.unsafe_get s (pos + 8) = 'm'
          && String.unsafe_get s (pos + 9) = 'i'
          && String.unsafe_get s (pos + 10) = 't'
        then 3
        else -1
      | _ -> -1
    in
    let i = Yojson.Safe.map_ident p f lb in
    Atdgen_runtime.Oj_run.read_until_field_value p lb;
    ( match i with
    | 0 ->
      field_ref := Atdgen_runtime.Oj_run.read_string p lb;
      bits0 := !bits0 lor 0x1
    | 1 ->
      field_after := Atdgen_runtime.Oj_run.read_string p lb;
      bits0 := !bits0 lor 0x2
    | 2 ->
      field_commits := read__2 p lb;
      bits0 := !bits0 lor 0x4
    | 3 ->
      field_head_commit := read_commit p lb;
      bits0 := !bits0 lor 0x8
    | 4 ->
      field_pusher := read_author p lb;
      bits0 := !bits0 lor 0x10
    | 5 ->
      field_sender := read_user p lb;
      bits0 := !bits0 lor 0x20
    | _ -> Yojson.Safe.skip_json p lb );
    while true do
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_sep p lb;
      Yojson.Safe.read_space p lb;
      let f s pos len =
        if pos < 0 || len < 0 || pos + len > String.length s then
          invalid_arg "out-of-bounds substring position or length";
        match len with
        | 3 ->
          if
            String.unsafe_get s pos = 'r' && String.unsafe_get s (pos + 1) = 'e' && String.unsafe_get s (pos + 2) = 'f'
          then 0
          else -1
        | 5 ->
          if
            String.unsafe_get s pos = 'a'
            && String.unsafe_get s (pos + 1) = 'f'
            && String.unsafe_get s (pos + 2) = 't'
            && String.unsafe_get s (pos + 3) = 'e'
            && String.unsafe_get s (pos + 4) = 'r'
          then 1
          else -1
        | 6 ->
          ( match String.unsafe_get s pos with
          | 'p' ->
            if
              String.unsafe_get s (pos + 1) = 'u'
              && String.unsafe_get s (pos + 2) = 's'
              && String.unsafe_get s (pos + 3) = 'h'
              && String.unsafe_get s (pos + 4) = 'e'
              && String.unsafe_get s (pos + 5) = 'r'
            then 4
            else -1
          | 's' ->
            if
              String.unsafe_get s (pos + 1) = 'e'
              && String.unsafe_get s (pos + 2) = 'n'
              && String.unsafe_get s (pos + 3) = 'd'
              && String.unsafe_get s (pos + 4) = 'e'
              && String.unsafe_get s (pos + 5) = 'r'
            then 5
            else -1
          | _ -> -1 )
        | 7 ->
          if
            String.unsafe_get s pos = 'c'
            && String.unsafe_get s (pos + 1) = 'o'
            && String.unsafe_get s (pos + 2) = 'm'
            && String.unsafe_get s (pos + 3) = 'm'
            && String.unsafe_get s (pos + 4) = 'i'
            && String.unsafe_get s (pos + 5) = 't'
            && String.unsafe_get s (pos + 6) = 's'
          then 2
          else -1
        | 11 ->
          if
            String.unsafe_get s pos = 'h'
            && String.unsafe_get s (pos + 1) = 'e'
            && String.unsafe_get s (pos + 2) = 'a'
            && String.unsafe_get s (pos + 3) = 'd'
            && String.unsafe_get s (pos + 4) = '_'
            && String.unsafe_get s (pos + 5) = 'c'
            && String.unsafe_get s (pos + 6) = 'o'
            && String.unsafe_get s (pos + 7) = 'm'
            && String.unsafe_get s (pos + 8) = 'm'
            && String.unsafe_get s (pos + 9) = 'i'
            && String.unsafe_get s (pos + 10) = 't'
          then 3
          else -1
        | _ -> -1
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      match i with
      | 0 ->
        field_ref := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x1
      | 1 ->
        field_after := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x2
      | 2 ->
        field_commits := read__2 p lb;
        bits0 := !bits0 lor 0x4
      | 3 ->
        field_head_commit := read_commit p lb;
        bits0 := !bits0 lor 0x8
      | 4 ->
        field_pusher := read_author p lb;
        bits0 := !bits0 lor 0x10
      | 5 ->
        field_sender := read_user p lb;
        bits0 := !bits0 lor 0x20
      | _ -> Yojson.Safe.skip_json p lb
    done;
    assert false
  with Yojson.End_of_object ->
    if !bits0 <> 0x3f then
      Atdgen_runtime.Oj_run.missing_fields p [| !bits0 |]
        [| "ref"; "after"; "commits"; "head_commit"; "pusher"; "sender" |];
    ( {
        ref = !field_ref;
        after = !field_after;
        commits = !field_commits;
        head_commit = !field_head_commit;
        pusher = !field_pusher;
        sender = !field_sender;
      }
      : commit_pushed_notification )

let commit_pushed_notification_of_string s =
  read_commit_pushed_notification (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write_branch : _ -> branch -> _ =
 fun ob x ->
  Bi_outbuf.add_char ob '{';
  let is_first = ref true in
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"name\":";
  Yojson.Safe.write_string ob x.name;
  Bi_outbuf.add_char ob '}'

let string_of_branch ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_branch ob x;
  Bi_outbuf.contents ob

let read_branch p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_lcurl p lb;
  let field_name = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let bits0 = ref 0 in
  try
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_object_end lb;
    Yojson.Safe.read_space p lb;
    let f s pos len =
      if pos < 0 || len < 0 || pos + len > String.length s then
        invalid_arg "out-of-bounds substring position or length";
      if
        len = 4
        && String.unsafe_get s pos = 'n'
        && String.unsafe_get s (pos + 1) = 'a'
        && String.unsafe_get s (pos + 2) = 'm'
        && String.unsafe_get s (pos + 3) = 'e'
      then 0
      else -1
    in
    let i = Yojson.Safe.map_ident p f lb in
    Atdgen_runtime.Oj_run.read_until_field_value p lb;
    ( match i with
    | 0 ->
      field_name := Atdgen_runtime.Oj_run.read_string p lb;
      bits0 := !bits0 lor 0x1
    | _ -> Yojson.Safe.skip_json p lb );
    while true do
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_sep p lb;
      Yojson.Safe.read_space p lb;
      let f s pos len =
        if pos < 0 || len < 0 || pos + len > String.length s then
          invalid_arg "out-of-bounds substring position or length";
        if
          len = 4
          && String.unsafe_get s pos = 'n'
          && String.unsafe_get s (pos + 1) = 'a'
          && String.unsafe_get s (pos + 2) = 'm'
          && String.unsafe_get s (pos + 3) = 'e'
        then 0
        else -1
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      match i with
      | 0 ->
        field_name := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x1
      | _ -> Yojson.Safe.skip_json p lb
    done;
    assert false
  with Yojson.End_of_object ->
    if !bits0 <> 0x1 then Atdgen_runtime.Oj_run.missing_fields p [| !bits0 |] [| "name" |];
    ({ name = !field_name } : branch)

let branch_of_string s = read_branch (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write__3 = Atdgen_runtime.Oj_run.write_list write_branch

let string_of__3 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__3 ob x;
  Bi_outbuf.contents ob

let read__3 = Atdgen_runtime.Oj_run.read_list read_branch

let _3_of_string s = read__3 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write_ci_build_notification : _ -> ci_build_notification -> _ =
 fun ob x ->
  Bi_outbuf.add_char ob '{';
  let is_first = ref true in
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"commit\":";
  write_commit ob x.commit;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"state\":";
  Yojson.Safe.write_string ob x.state;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"target_url\":";
  Yojson.Safe.write_string ob x.target_url;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"branches\":";
  write__3 ob x.branches;
  Bi_outbuf.add_char ob '}'

let string_of_ci_build_notification ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_ci_build_notification ob x;
  Bi_outbuf.contents ob

let read_ci_build_notification p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_lcurl p lb;
  let field_commit = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_state = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_target_url = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_branches = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let bits0 = ref 0 in
  try
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_object_end lb;
    Yojson.Safe.read_space p lb;
    let f s pos len =
      if pos < 0 || len < 0 || pos + len > String.length s then
        invalid_arg "out-of-bounds substring position or length";
      match len with
      | 5 ->
        if
          String.unsafe_get s pos = 's'
          && String.unsafe_get s (pos + 1) = 't'
          && String.unsafe_get s (pos + 2) = 'a'
          && String.unsafe_get s (pos + 3) = 't'
          && String.unsafe_get s (pos + 4) = 'e'
        then 1
        else -1
      | 6 ->
        if
          String.unsafe_get s pos = 'c'
          && String.unsafe_get s (pos + 1) = 'o'
          && String.unsafe_get s (pos + 2) = 'm'
          && String.unsafe_get s (pos + 3) = 'm'
          && String.unsafe_get s (pos + 4) = 'i'
          && String.unsafe_get s (pos + 5) = 't'
        then 0
        else -1
      | 8 ->
        if
          String.unsafe_get s pos = 'b'
          && String.unsafe_get s (pos + 1) = 'r'
          && String.unsafe_get s (pos + 2) = 'a'
          && String.unsafe_get s (pos + 3) = 'n'
          && String.unsafe_get s (pos + 4) = 'c'
          && String.unsafe_get s (pos + 5) = 'h'
          && String.unsafe_get s (pos + 6) = 'e'
          && String.unsafe_get s (pos + 7) = 's'
        then 3
        else -1
      | 10 ->
        if
          String.unsafe_get s pos = 't'
          && String.unsafe_get s (pos + 1) = 'a'
          && String.unsafe_get s (pos + 2) = 'r'
          && String.unsafe_get s (pos + 3) = 'g'
          && String.unsafe_get s (pos + 4) = 'e'
          && String.unsafe_get s (pos + 5) = 't'
          && String.unsafe_get s (pos + 6) = '_'
          && String.unsafe_get s (pos + 7) = 'u'
          && String.unsafe_get s (pos + 8) = 'r'
          && String.unsafe_get s (pos + 9) = 'l'
        then 2
        else -1
      | _ -> -1
    in
    let i = Yojson.Safe.map_ident p f lb in
    Atdgen_runtime.Oj_run.read_until_field_value p lb;
    ( match i with
    | 0 ->
      field_commit := read_commit p lb;
      bits0 := !bits0 lor 0x1
    | 1 ->
      field_state := Atdgen_runtime.Oj_run.read_string p lb;
      bits0 := !bits0 lor 0x2
    | 2 ->
      field_target_url := Atdgen_runtime.Oj_run.read_string p lb;
      bits0 := !bits0 lor 0x4
    | 3 ->
      field_branches := read__3 p lb;
      bits0 := !bits0 lor 0x8
    | _ -> Yojson.Safe.skip_json p lb );
    while true do
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_sep p lb;
      Yojson.Safe.read_space p lb;
      let f s pos len =
        if pos < 0 || len < 0 || pos + len > String.length s then
          invalid_arg "out-of-bounds substring position or length";
        match len with
        | 5 ->
          if
            String.unsafe_get s pos = 's'
            && String.unsafe_get s (pos + 1) = 't'
            && String.unsafe_get s (pos + 2) = 'a'
            && String.unsafe_get s (pos + 3) = 't'
            && String.unsafe_get s (pos + 4) = 'e'
          then 1
          else -1
        | 6 ->
          if
            String.unsafe_get s pos = 'c'
            && String.unsafe_get s (pos + 1) = 'o'
            && String.unsafe_get s (pos + 2) = 'm'
            && String.unsafe_get s (pos + 3) = 'm'
            && String.unsafe_get s (pos + 4) = 'i'
            && String.unsafe_get s (pos + 5) = 't'
          then 0
          else -1
        | 8 ->
          if
            String.unsafe_get s pos = 'b'
            && String.unsafe_get s (pos + 1) = 'r'
            && String.unsafe_get s (pos + 2) = 'a'
            && String.unsafe_get s (pos + 3) = 'n'
            && String.unsafe_get s (pos + 4) = 'c'
            && String.unsafe_get s (pos + 5) = 'h'
            && String.unsafe_get s (pos + 6) = 'e'
            && String.unsafe_get s (pos + 7) = 's'
          then 3
          else -1
        | 10 ->
          if
            String.unsafe_get s pos = 't'
            && String.unsafe_get s (pos + 1) = 'a'
            && String.unsafe_get s (pos + 2) = 'r'
            && String.unsafe_get s (pos + 3) = 'g'
            && String.unsafe_get s (pos + 4) = 'e'
            && String.unsafe_get s (pos + 5) = 't'
            && String.unsafe_get s (pos + 6) = '_'
            && String.unsafe_get s (pos + 7) = 'u'
            && String.unsafe_get s (pos + 8) = 'r'
            && String.unsafe_get s (pos + 9) = 'l'
          then 2
          else -1
        | _ -> -1
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      match i with
      | 0 ->
        field_commit := read_commit p lb;
        bits0 := !bits0 lor 0x1
      | 1 ->
        field_state := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x2
      | 2 ->
        field_target_url := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x4
      | 3 ->
        field_branches := read__3 p lb;
        bits0 := !bits0 lor 0x8
      | _ -> Yojson.Safe.skip_json p lb
    done;
    assert false
  with Yojson.End_of_object ->
    if !bits0 <> 0xf then
      Atdgen_runtime.Oj_run.missing_fields p [| !bits0 |] [| "commit"; "state"; "target_url"; "branches" |];
    ( { commit = !field_commit; state = !field_state; target_url = !field_target_url; branches = !field_branches }
      : ci_build_notification )

let ci_build_notification_of_string s = read_ci_build_notification (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
