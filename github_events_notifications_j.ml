(* Auto-generated from "github_events_notifications.atd" *)
[@@@ocaml.warning "-27-32-35-39"]

type user = Github_events_notifications_t.user = {
  login : string;
  id : int;
}

type pull_request = Github_events_notifications_t.pull_request = {
  id : int;
  body : string;
  title : string;
  url : string;
}

type pr_action = Github_events_notifications_t.pr_action

type pr_notification = Github_events_notifications_t.pr_notification = {
  action : pr_action;
  number : int;
  sender : user;
  pull_request : pull_request;
}

type author = Github_events_notifications_t.author = {
  name : string;
  email : string;
  username : string option;
}

type inner_commit = Github_events_notifications_t.inner_commit = {
  author : author;
  committer : author;
  message : string;
}

type commit_hash = Github_events_notifications_t.commit_hash

type commit = Github_events_notifications_t.commit = {
  id : commit_hash;
  message : string;
  timestamp : string;
  url : string;
  author : author;
  committer : author;
}

type commit_pushed_notification = Github_events_notifications_t.commit_pushed_notification = {
  ref : string;
  after : commit_hash;
  commits : commit list;
  head_commit : commit;
  pusher : author;
  sender : user;
}

type ci_commit = Github_events_notifications_t.ci_commit = {
  sha : commit_hash;
  commit : inner_commit;
  url (*atd html_url *) : string;
}

type ci_build_state = Github_events_notifications_t.ci_build_state

type branch = Github_events_notifications_t.branch = { name : string }

type ci_build_notification = Github_events_notifications_t.ci_build_notification = {
  commit : ci_commit;
  state : ci_build_state;
  target_url : string;
  branches : branch list;
}

let write_user : _ -> user -> _ =
 fun ob x ->
  Bi_outbuf.add_char ob '{';
  let is_first = ref true in
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"login\":";
  Yojson.Safe.write_string ob x.login;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"id\":";
  Yojson.Safe.write_int ob x.id;
  Bi_outbuf.add_char ob '}'

let string_of_user ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_user ob x;
  Bi_outbuf.contents ob

let read_user p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_lcurl p lb;
  let field_login = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_id = ref (Obj.magic (Sys.opaque_identity 0.0)) in
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
      field_login := Atdgen_runtime.Oj_run.read_string p lb;
      bits0 := !bits0 lor 0x1
    | 1 ->
      field_id := Atdgen_runtime.Oj_run.read_int p lb;
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
        field_login := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x1
      | 1 ->
        field_id := Atdgen_runtime.Oj_run.read_int p lb;
        bits0 := !bits0 lor 0x2
      | _ -> Yojson.Safe.skip_json p lb
    done;
    assert false
  with Yojson.End_of_object ->
    if !bits0 <> 0x3 then Atdgen_runtime.Oj_run.missing_fields p [| !bits0 |] [| "login"; "id" |];
    ({ login = !field_login; id = !field_id } : user)

let user_of_string s = read_user (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write_pull_request : _ -> pull_request -> _ =
 fun ob x ->
  Bi_outbuf.add_char ob '{';
  let is_first = ref true in
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"id\":";
  Yojson.Safe.write_int ob x.id;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"body\":";
  Yojson.Safe.write_string ob x.body;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"title\":";
  Yojson.Safe.write_string ob x.title;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"url\":";
  Yojson.Safe.write_string ob x.url;
  Bi_outbuf.add_char ob '}'

let string_of_pull_request ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_pull_request ob x;
  Bi_outbuf.contents ob

let read_pull_request p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_lcurl p lb;
  let field_id = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_body = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_title = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_url = ref (Obj.magic (Sys.opaque_identity 0.0)) in
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
      field_id := Atdgen_runtime.Oj_run.read_int p lb;
      bits0 := !bits0 lor 0x1
    | 1 ->
      field_body := Atdgen_runtime.Oj_run.read_string p lb;
      bits0 := !bits0 lor 0x2
    | 2 ->
      field_title := Atdgen_runtime.Oj_run.read_string p lb;
      bits0 := !bits0 lor 0x4
    | 3 ->
      field_url := Atdgen_runtime.Oj_run.read_string p lb;
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
        field_id := Atdgen_runtime.Oj_run.read_int p lb;
        bits0 := !bits0 lor 0x1
      | 1 ->
        field_body := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x2
      | 2 ->
        field_title := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x4
      | 3 ->
        field_url := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x8
      | _ -> Yojson.Safe.skip_json p lb
    done;
    assert false
  with Yojson.End_of_object ->
    if !bits0 <> 0xf then Atdgen_runtime.Oj_run.missing_fields p [| !bits0 |] [| "id"; "body"; "title"; "url" |];
    ({ id = !field_id; body = !field_body; title = !field_title; url = !field_url } : pull_request)

let pull_request_of_string s = read_pull_request (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write_pr_action ob x =
  match x with
  | `Assigned -> Bi_outbuf.add_string ob "<\"assigned\">"
  | `Unassigned -> Bi_outbuf.add_string ob "<\"unassigned\">"
  | `Review_requested -> Bi_outbuf.add_string ob "<\"review_requested\">"
  | `Review_request_removed -> Bi_outbuf.add_string ob "<\"review_request_removed\">"
  | `Labeled -> Bi_outbuf.add_string ob "<\"labeled\">"
  | `Unlabeled -> Bi_outbuf.add_string ob "<\"unlabeled\">"
  | `Opened -> Bi_outbuf.add_string ob "<\"opened\">"
  | `Edited -> Bi_outbuf.add_string ob "<\"edited\">"
  | `Closed -> Bi_outbuf.add_string ob "<\"closed\">"
  | `Ready_for_review -> Bi_outbuf.add_string ob "<\"ready_for_review\">"
  | `Locked -> Bi_outbuf.add_string ob "<\"locked\">"
  | `Unlocked -> Bi_outbuf.add_string ob "<\"unlocked\">"
  | `Reopened -> Bi_outbuf.add_string ob "<\"reopened\">"

let string_of_pr_action ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_pr_action ob x;
  Bi_outbuf.contents ob

let read_pr_action p lb =
  Yojson.Safe.read_space p lb;
  match Yojson.Safe.start_any_variant p lb with
  | `Edgy_bracket ->
    ( match Yojson.Safe.read_ident p lb with
    | "assigned" ->
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_gt p lb;
      `Assigned
    | "unassigned" ->
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_gt p lb;
      `Unassigned
    | "review_requested" ->
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_gt p lb;
      `Review_requested
    | "review_request_removed" ->
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_gt p lb;
      `Review_request_removed
    | "labeled" ->
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_gt p lb;
      `Labeled
    | "unlabeled" ->
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_gt p lb;
      `Unlabeled
    | "opened" ->
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_gt p lb;
      `Opened
    | "edited" ->
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_gt p lb;
      `Edited
    | "closed" ->
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_gt p lb;
      `Closed
    | "ready_for_review" ->
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_gt p lb;
      `Ready_for_review
    | "locked" ->
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_gt p lb;
      `Locked
    | "unlocked" ->
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_gt p lb;
      `Unlocked
    | "reopened" ->
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_gt p lb;
      `Reopened
    | x -> Atdgen_runtime.Oj_run.invalid_variant_tag p x )
  | `Double_quote ->
    ( match Yojson.Safe.finish_string p lb with
    | "assigned" -> `Assigned
    | "unassigned" -> `Unassigned
    | "review_requested" -> `Review_requested
    | "review_request_removed" -> `Review_request_removed
    | "labeled" -> `Labeled
    | "unlabeled" -> `Unlabeled
    | "opened" -> `Opened
    | "edited" -> `Edited
    | "closed" -> `Closed
    | "ready_for_review" -> `Ready_for_review
    | "locked" -> `Locked
    | "unlocked" -> `Unlocked
    | "reopened" -> `Reopened
    | x -> Atdgen_runtime.Oj_run.invalid_variant_tag p x )
  | `Square_bracket ->
    ( match Atdgen_runtime.Oj_run.read_string p lb with
    | x -> Atdgen_runtime.Oj_run.invalid_variant_tag p x )

let pr_action_of_string s = read_pr_action (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write_pr_notification : _ -> pr_notification -> _ =
 fun ob x ->
  Bi_outbuf.add_char ob '{';
  let is_first = ref true in
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"action\":";
  write_pr_action ob x.action;
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
      field_action := read_pr_action p lb;
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
        field_action := read_pr_action p lb;
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

let write__1 = Atdgen_runtime.Oj_run.write_nullable Yojson.Safe.write_string

let string_of__1 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__1 ob x;
  Bi_outbuf.contents ob

let read__1 p lb =
  Yojson.Safe.read_space p lb;
  (if Yojson.Safe.read_null_if_possible p lb then None else Some (Atdgen_runtime.Oj_run.read_string p lb) : _ option)

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

let write_inner_commit : _ -> inner_commit -> _ =
 fun ob x ->
  Bi_outbuf.add_char ob '{';
  let is_first = ref true in
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"author\":";
  write_author ob x.author;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"committer\":";
  write_author ob x.committer;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"message\":";
  Yojson.Safe.write_string ob x.message;
  Bi_outbuf.add_char ob '}'

let string_of_inner_commit ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_inner_commit ob x;
  Bi_outbuf.contents ob

let read_inner_commit p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_lcurl p lb;
  let field_author = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_committer = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_message = ref (Obj.magic (Sys.opaque_identity 0.0)) in
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
        if
          String.unsafe_get s pos = 'a'
          && String.unsafe_get s (pos + 1) = 'u'
          && String.unsafe_get s (pos + 2) = 't'
          && String.unsafe_get s (pos + 3) = 'h'
          && String.unsafe_get s (pos + 4) = 'o'
          && String.unsafe_get s (pos + 5) = 'r'
        then 0
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
        then 2
        else -1
      | 9 ->
        if
          String.unsafe_get s pos = 'c'
          && String.unsafe_get s (pos + 1) = 'o'
          && String.unsafe_get s (pos + 2) = 'm'
          && String.unsafe_get s (pos + 3) = 'm'
          && String.unsafe_get s (pos + 4) = 'i'
          && String.unsafe_get s (pos + 5) = 't'
          && String.unsafe_get s (pos + 6) = 't'
          && String.unsafe_get s (pos + 7) = 'e'
          && String.unsafe_get s (pos + 8) = 'r'
        then 1
        else -1
      | _ -> -1
    in
    let i = Yojson.Safe.map_ident p f lb in
    Atdgen_runtime.Oj_run.read_until_field_value p lb;
    ( match i with
    | 0 ->
      field_author := read_author p lb;
      bits0 := !bits0 lor 0x1
    | 1 ->
      field_committer := read_author p lb;
      bits0 := !bits0 lor 0x2
    | 2 ->
      field_message := Atdgen_runtime.Oj_run.read_string p lb;
      bits0 := !bits0 lor 0x4
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
          if
            String.unsafe_get s pos = 'a'
            && String.unsafe_get s (pos + 1) = 'u'
            && String.unsafe_get s (pos + 2) = 't'
            && String.unsafe_get s (pos + 3) = 'h'
            && String.unsafe_get s (pos + 4) = 'o'
            && String.unsafe_get s (pos + 5) = 'r'
          then 0
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
          then 2
          else -1
        | 9 ->
          if
            String.unsafe_get s pos = 'c'
            && String.unsafe_get s (pos + 1) = 'o'
            && String.unsafe_get s (pos + 2) = 'm'
            && String.unsafe_get s (pos + 3) = 'm'
            && String.unsafe_get s (pos + 4) = 'i'
            && String.unsafe_get s (pos + 5) = 't'
            && String.unsafe_get s (pos + 6) = 't'
            && String.unsafe_get s (pos + 7) = 'e'
            && String.unsafe_get s (pos + 8) = 'r'
          then 1
          else -1
        | _ -> -1
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      match i with
      | 0 ->
        field_author := read_author p lb;
        bits0 := !bits0 lor 0x1
      | 1 ->
        field_committer := read_author p lb;
        bits0 := !bits0 lor 0x2
      | 2 ->
        field_message := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x4
      | _ -> Yojson.Safe.skip_json p lb
    done;
    assert false
  with Yojson.End_of_object ->
    if !bits0 <> 0x7 then Atdgen_runtime.Oj_run.missing_fields p [| !bits0 |] [| "author"; "committer"; "message" |];
    ({ author = !field_author; committer = !field_committer; message = !field_message } : inner_commit)

let inner_commit_of_string s = read_inner_commit (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write__2 ob x =
  let x = Sha1.to_hex x in
  Yojson.Safe.write_string ob x

let string_of__2 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__2 ob x;
  Bi_outbuf.contents ob

let read__2 p lb =
  let x = Atdgen_runtime.Oj_run.read_string p lb in
  Sha1.string x

let _2_of_string s = read__2 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write_commit_hash = write__2

let string_of_commit_hash ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_commit_hash ob x;
  Bi_outbuf.contents ob

let read_commit_hash = read__2

let commit_hash_of_string s = read_commit_hash (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write_commit : _ -> commit -> _ =
 fun ob x ->
  Bi_outbuf.add_char ob '{';
  let is_first = ref true in
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"id\":";
  write_commit_hash ob x.id;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"message\":";
  Yojson.Safe.write_string ob x.message;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"timestamp\":";
  Yojson.Safe.write_string ob x.timestamp;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"url\":";
  Yojson.Safe.write_string ob x.url;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"author\":";
  write_author ob x.author;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"committer\":";
  write_author ob x.committer;
  Bi_outbuf.add_char ob '}'

let string_of_commit ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_commit ob x;
  Bi_outbuf.contents ob

let read_commit p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_lcurl p lb;
  let field_id = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_message = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_timestamp = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_url = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_author = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_committer = ref (Obj.magic (Sys.opaque_identity 0.0)) in
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
      field_id := read_commit_hash p lb;
      bits0 := !bits0 lor 0x1
    | 1 ->
      field_message := Atdgen_runtime.Oj_run.read_string p lb;
      bits0 := !bits0 lor 0x2
    | 2 ->
      field_timestamp := Atdgen_runtime.Oj_run.read_string p lb;
      bits0 := !bits0 lor 0x4
    | 3 ->
      field_url := Atdgen_runtime.Oj_run.read_string p lb;
      bits0 := !bits0 lor 0x8
    | 4 ->
      field_author := read_author p lb;
      bits0 := !bits0 lor 0x10
    | 5 ->
      field_committer := read_author p lb;
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
        field_id := read_commit_hash p lb;
        bits0 := !bits0 lor 0x1
      | 1 ->
        field_message := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x2
      | 2 ->
        field_timestamp := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x4
      | 3 ->
        field_url := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x8
      | 4 ->
        field_author := read_author p lb;
        bits0 := !bits0 lor 0x10
      | 5 ->
        field_committer := read_author p lb;
        bits0 := !bits0 lor 0x20
      | _ -> Yojson.Safe.skip_json p lb
    done;
    assert false
  with Yojson.End_of_object ->
    if !bits0 <> 0x3f then
      Atdgen_runtime.Oj_run.missing_fields p [| !bits0 |]
        [| "id"; "message"; "timestamp"; "url"; "author"; "committer" |];
    ( {
        id = !field_id;
        message = !field_message;
        timestamp = !field_timestamp;
        url = !field_url;
        author = !field_author;
        committer = !field_committer;
      }
      : commit )

let commit_of_string s = read_commit (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write__3 = Atdgen_runtime.Oj_run.write_list write_commit

let string_of__3 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__3 ob x;
  Bi_outbuf.contents ob

let read__3 = Atdgen_runtime.Oj_run.read_list read_commit

let _3_of_string s = read__3 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write_commit_pushed_notification : _ -> commit_pushed_notification -> _ =
 fun ob x ->
  Bi_outbuf.add_char ob '{';
  let is_first = ref true in
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"ref\":";
  Yojson.Safe.write_string ob x.ref;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"after\":";
  write_commit_hash ob x.after;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"commits\":";
  write__3 ob x.commits;
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
      field_after := read_commit_hash p lb;
      bits0 := !bits0 lor 0x2
    | 2 ->
      field_commits := read__3 p lb;
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
        field_after := read_commit_hash p lb;
        bits0 := !bits0 lor 0x2
      | 2 ->
        field_commits := read__3 p lb;
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

let write_ci_commit : _ -> ci_commit -> _ =
 fun ob x ->
  Bi_outbuf.add_char ob '{';
  let is_first = ref true in
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"sha\":";
  write_commit_hash ob x.sha;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"commit\":";
  write_inner_commit ob x.commit;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"html_url\":";
  Yojson.Safe.write_string ob x.url;
  Bi_outbuf.add_char ob '}'

let string_of_ci_commit ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_ci_commit ob x;
  Bi_outbuf.contents ob

let read_ci_commit p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_lcurl p lb;
  let field_sha = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_commit = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_url = ref (Obj.magic (Sys.opaque_identity 0.0)) in
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
        if String.unsafe_get s pos = 's' && String.unsafe_get s (pos + 1) = 'h' && String.unsafe_get s (pos + 2) = 'a'
        then 0
        else -1
      | 6 ->
        if
          String.unsafe_get s pos = 'c'
          && String.unsafe_get s (pos + 1) = 'o'
          && String.unsafe_get s (pos + 2) = 'm'
          && String.unsafe_get s (pos + 3) = 'm'
          && String.unsafe_get s (pos + 4) = 'i'
          && String.unsafe_get s (pos + 5) = 't'
        then 1
        else -1
      | 8 ->
        if
          String.unsafe_get s pos = 'h'
          && String.unsafe_get s (pos + 1) = 't'
          && String.unsafe_get s (pos + 2) = 'm'
          && String.unsafe_get s (pos + 3) = 'l'
          && String.unsafe_get s (pos + 4) = '_'
          && String.unsafe_get s (pos + 5) = 'u'
          && String.unsafe_get s (pos + 6) = 'r'
          && String.unsafe_get s (pos + 7) = 'l'
        then 2
        else -1
      | _ -> -1
    in
    let i = Yojson.Safe.map_ident p f lb in
    Atdgen_runtime.Oj_run.read_until_field_value p lb;
    ( match i with
    | 0 ->
      field_sha := read_commit_hash p lb;
      bits0 := !bits0 lor 0x1
    | 1 ->
      field_commit := read_inner_commit p lb;
      bits0 := !bits0 lor 0x2
    | 2 ->
      field_url := Atdgen_runtime.Oj_run.read_string p lb;
      bits0 := !bits0 lor 0x4
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
            String.unsafe_get s pos = 's' && String.unsafe_get s (pos + 1) = 'h' && String.unsafe_get s (pos + 2) = 'a'
          then 0
          else -1
        | 6 ->
          if
            String.unsafe_get s pos = 'c'
            && String.unsafe_get s (pos + 1) = 'o'
            && String.unsafe_get s (pos + 2) = 'm'
            && String.unsafe_get s (pos + 3) = 'm'
            && String.unsafe_get s (pos + 4) = 'i'
            && String.unsafe_get s (pos + 5) = 't'
          then 1
          else -1
        | 8 ->
          if
            String.unsafe_get s pos = 'h'
            && String.unsafe_get s (pos + 1) = 't'
            && String.unsafe_get s (pos + 2) = 'm'
            && String.unsafe_get s (pos + 3) = 'l'
            && String.unsafe_get s (pos + 4) = '_'
            && String.unsafe_get s (pos + 5) = 'u'
            && String.unsafe_get s (pos + 6) = 'r'
            && String.unsafe_get s (pos + 7) = 'l'
          then 2
          else -1
        | _ -> -1
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      match i with
      | 0 ->
        field_sha := read_commit_hash p lb;
        bits0 := !bits0 lor 0x1
      | 1 ->
        field_commit := read_inner_commit p lb;
        bits0 := !bits0 lor 0x2
      | 2 ->
        field_url := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x4
      | _ -> Yojson.Safe.skip_json p lb
    done;
    assert false
  with Yojson.End_of_object ->
    if !bits0 <> 0x7 then Atdgen_runtime.Oj_run.missing_fields p [| !bits0 |] [| "sha"; "commit"; "html_url" |];
    ({ sha = !field_sha; commit = !field_commit; url = !field_url } : ci_commit)

let ci_commit_of_string s = read_ci_commit (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write_ci_build_state ob x =
  match x with
  | `Success -> Bi_outbuf.add_string ob "<\"success\">"
  | `Failure -> Bi_outbuf.add_string ob "<\"failure\">"
  | `Neutral -> Bi_outbuf.add_string ob "<\"neutral\">"
  | `Cancelled -> Bi_outbuf.add_string ob "<\"cancelled\">"
  | `Timed_out -> Bi_outbuf.add_string ob "<\"timed_out\">"
  | `Action_required -> Bi_outbuf.add_string ob "<\"action_required\">"

let string_of_ci_build_state ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_ci_build_state ob x;
  Bi_outbuf.contents ob

let read_ci_build_state p lb =
  Yojson.Safe.read_space p lb;
  match Yojson.Safe.start_any_variant p lb with
  | `Edgy_bracket ->
    ( match Yojson.Safe.read_ident p lb with
    | "success" ->
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_gt p lb;
      `Success
    | "failure" ->
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_gt p lb;
      `Failure
    | "neutral" ->
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_gt p lb;
      `Neutral
    | "cancelled" ->
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_gt p lb;
      `Cancelled
    | "timed_out" ->
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_gt p lb;
      `Timed_out
    | "action_required" ->
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_gt p lb;
      `Action_required
    | x -> Atdgen_runtime.Oj_run.invalid_variant_tag p x )
  | `Double_quote ->
    ( match Yojson.Safe.finish_string p lb with
    | "success" -> `Success
    | "failure" -> `Failure
    | "neutral" -> `Neutral
    | "cancelled" -> `Cancelled
    | "timed_out" -> `Timed_out
    | "action_required" -> `Action_required
    | x -> Atdgen_runtime.Oj_run.invalid_variant_tag p x )
  | `Square_bracket ->
    ( match Atdgen_runtime.Oj_run.read_string p lb with
    | x -> Atdgen_runtime.Oj_run.invalid_variant_tag p x )

let ci_build_state_of_string s = read_ci_build_state (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

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

let write__4 = Atdgen_runtime.Oj_run.write_list write_branch

let string_of__4 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__4 ob x;
  Bi_outbuf.contents ob

let read__4 = Atdgen_runtime.Oj_run.read_list read_branch

let _4_of_string s = read__4 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write_ci_build_notification : _ -> ci_build_notification -> _ =
 fun ob x ->
  Bi_outbuf.add_char ob '{';
  let is_first = ref true in
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"commit\":";
  write_ci_commit ob x.commit;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"state\":";
  write_ci_build_state ob x.state;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"target_url\":";
  Yojson.Safe.write_string ob x.target_url;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"branches\":";
  write__4 ob x.branches;
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
      field_commit := read_ci_commit p lb;
      bits0 := !bits0 lor 0x1
    | 1 ->
      field_state := read_ci_build_state p lb;
      bits0 := !bits0 lor 0x2
    | 2 ->
      field_target_url := Atdgen_runtime.Oj_run.read_string p lb;
      bits0 := !bits0 lor 0x4
    | 3 ->
      field_branches := read__4 p lb;
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
        field_commit := read_ci_commit p lb;
        bits0 := !bits0 lor 0x1
      | 1 ->
        field_state := read_ci_build_state p lb;
        bits0 := !bits0 lor 0x2
      | 2 ->
        field_target_url := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x4
      | 3 ->
        field_branches := read__4 p lb;
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
