(* Auto-generated from "notabot_github.atd" *)
[@@@ocaml.warning "-27-32-35-39"]

type user = Notabot_github_t.user = {
  login : string;
  id : int;
}

type success_status = Notabot_github_t.success_status =
  | Success
  | Failed

type pr_review_requested = Notabot_github_t.pr_review_requested = {
  author : string;
  title : string;
  description : string;
  link : string;
}

type commit_hash = Notabot_github_t.commit_hash

type author = Notabot_github_t.author = {
  name : string;
  email : string;
}

type commit_pushed = Notabot_github_t.commit_pushed = {
  author : author;
  hash : commit_hash;
  commit_message : string;
  link : string;
}

type commit = Notabot_github_t.commit = {
  sha : commit_hash;
  message : string;
  url : string;
  author : author;
}

type ci_build_status_changed = Notabot_github_t.ci_build_status_changed = {
  commit : commit;
  build_status : success_status;
  branch : string;
  link : string;
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

let write_success_status : _ -> success_status -> _ =
 fun ob x ->
  match x with
  | Success -> Bi_outbuf.add_string ob "<\"success\">"
  | Failed -> Bi_outbuf.add_string ob "<\"failure\">"

let string_of_success_status ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_success_status ob x;
  Bi_outbuf.contents ob

let read_success_status p lb =
  Yojson.Safe.read_space p lb;
  match Yojson.Safe.start_any_variant p lb with
  | `Edgy_bracket ->
    ( match Yojson.Safe.read_ident p lb with
    | "success" ->
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_gt p lb;
      (Success : success_status)
    | "failure" ->
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_gt p lb;
      (Failed : success_status)
    | x -> Atdgen_runtime.Oj_run.invalid_variant_tag p x )
  | `Double_quote ->
    ( match Yojson.Safe.finish_string p lb with
    | "success" -> (Success : success_status)
    | "failure" -> (Failed : success_status)
    | x -> Atdgen_runtime.Oj_run.invalid_variant_tag p x )
  | `Square_bracket ->
    ( match Atdgen_runtime.Oj_run.read_string p lb with
    | x -> Atdgen_runtime.Oj_run.invalid_variant_tag p x )

let success_status_of_string s = read_success_status (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write_pr_review_requested : _ -> pr_review_requested -> _ =
 fun ob x ->
  Bi_outbuf.add_char ob '{';
  let is_first = ref true in
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"author\":";
  Yojson.Safe.write_string ob x.author;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"title\":";
  Yojson.Safe.write_string ob x.title;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"description\":";
  Yojson.Safe.write_string ob x.description;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"link\":";
  Yojson.Safe.write_string ob x.link;
  Bi_outbuf.add_char ob '}'

let string_of_pr_review_requested ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_pr_review_requested ob x;
  Bi_outbuf.contents ob

let read_pr_review_requested p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_lcurl p lb;
  let field_author = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_title = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_description = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_link = ref (Obj.magic (Sys.opaque_identity 0.0)) in
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
          String.unsafe_get s pos = 'l'
          && String.unsafe_get s (pos + 1) = 'i'
          && String.unsafe_get s (pos + 2) = 'n'
          && String.unsafe_get s (pos + 3) = 'k'
        then 3
        else -1
      | 5 ->
        if
          String.unsafe_get s pos = 't'
          && String.unsafe_get s (pos + 1) = 'i'
          && String.unsafe_get s (pos + 2) = 't'
          && String.unsafe_get s (pos + 3) = 'l'
          && String.unsafe_get s (pos + 4) = 'e'
        then 1
        else -1
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
      | 11 ->
        if
          String.unsafe_get s pos = 'd'
          && String.unsafe_get s (pos + 1) = 'e'
          && String.unsafe_get s (pos + 2) = 's'
          && String.unsafe_get s (pos + 3) = 'c'
          && String.unsafe_get s (pos + 4) = 'r'
          && String.unsafe_get s (pos + 5) = 'i'
          && String.unsafe_get s (pos + 6) = 'p'
          && String.unsafe_get s (pos + 7) = 't'
          && String.unsafe_get s (pos + 8) = 'i'
          && String.unsafe_get s (pos + 9) = 'o'
          && String.unsafe_get s (pos + 10) = 'n'
        then 2
        else -1
      | _ -> -1
    in
    let i = Yojson.Safe.map_ident p f lb in
    Atdgen_runtime.Oj_run.read_until_field_value p lb;
    ( match i with
    | 0 ->
      field_author := Atdgen_runtime.Oj_run.read_string p lb;
      bits0 := !bits0 lor 0x1
    | 1 ->
      field_title := Atdgen_runtime.Oj_run.read_string p lb;
      bits0 := !bits0 lor 0x2
    | 2 ->
      field_description := Atdgen_runtime.Oj_run.read_string p lb;
      bits0 := !bits0 lor 0x4
    | 3 ->
      field_link := Atdgen_runtime.Oj_run.read_string p lb;
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
        | 4 ->
          if
            String.unsafe_get s pos = 'l'
            && String.unsafe_get s (pos + 1) = 'i'
            && String.unsafe_get s (pos + 2) = 'n'
            && String.unsafe_get s (pos + 3) = 'k'
          then 3
          else -1
        | 5 ->
          if
            String.unsafe_get s pos = 't'
            && String.unsafe_get s (pos + 1) = 'i'
            && String.unsafe_get s (pos + 2) = 't'
            && String.unsafe_get s (pos + 3) = 'l'
            && String.unsafe_get s (pos + 4) = 'e'
          then 1
          else -1
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
        | 11 ->
          if
            String.unsafe_get s pos = 'd'
            && String.unsafe_get s (pos + 1) = 'e'
            && String.unsafe_get s (pos + 2) = 's'
            && String.unsafe_get s (pos + 3) = 'c'
            && String.unsafe_get s (pos + 4) = 'r'
            && String.unsafe_get s (pos + 5) = 'i'
            && String.unsafe_get s (pos + 6) = 'p'
            && String.unsafe_get s (pos + 7) = 't'
            && String.unsafe_get s (pos + 8) = 'i'
            && String.unsafe_get s (pos + 9) = 'o'
            && String.unsafe_get s (pos + 10) = 'n'
          then 2
          else -1
        | _ -> -1
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      match i with
      | 0 ->
        field_author := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x1
      | 1 ->
        field_title := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x2
      | 2 ->
        field_description := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x4
      | 3 ->
        field_link := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x8
      | _ -> Yojson.Safe.skip_json p lb
    done;
    assert false
  with Yojson.End_of_object ->
    if !bits0 <> 0xf then
      Atdgen_runtime.Oj_run.missing_fields p [| !bits0 |] [| "author"; "title"; "description"; "link" |];
    ( { author = !field_author; title = !field_title; description = !field_description; link = !field_link }
      : pr_review_requested )

let pr_review_requested_of_string s = read_pr_review_requested (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write__1 ob x =
  let x = Sha1.to_hex x in
  Yojson.Safe.write_string ob x

let string_of__1 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__1 ob x;
  Bi_outbuf.contents ob

let read__1 p lb =
  let x = Atdgen_runtime.Oj_run.read_string p lb in
  Sha1.string x

let _1_of_string s = read__1 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write_commit_hash = write__1

let string_of_commit_hash ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_commit_hash ob x;
  Bi_outbuf.contents ob

let read_commit_hash = read__1

let commit_hash_of_string s = read_commit_hash (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

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
      | _ -> Yojson.Safe.skip_json p lb
    done;
    assert false
  with Yojson.End_of_object ->
    if !bits0 <> 0x3 then Atdgen_runtime.Oj_run.missing_fields p [| !bits0 |] [| "name"; "email" |];
    ({ name = !field_name; email = !field_email } : author)

let author_of_string s = read_author (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write_commit_pushed : _ -> commit_pushed -> _ =
 fun ob x ->
  Bi_outbuf.add_char ob '{';
  let is_first = ref true in
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"author\":";
  write_author ob x.author;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"hash\":";
  write_commit_hash ob x.hash;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"commit_message\":";
  Yojson.Safe.write_string ob x.commit_message;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"link\":";
  Yojson.Safe.write_string ob x.link;
  Bi_outbuf.add_char ob '}'

let string_of_commit_pushed ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_commit_pushed ob x;
  Bi_outbuf.contents ob

let read_commit_pushed p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_lcurl p lb;
  let field_author = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_hash = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_commit_message = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_link = ref (Obj.magic (Sys.opaque_identity 0.0)) in
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
        ( match String.unsafe_get s pos with
        | 'h' ->
          if
            String.unsafe_get s (pos + 1) = 'a'
            && String.unsafe_get s (pos + 2) = 's'
            && String.unsafe_get s (pos + 3) = 'h'
          then 1
          else -1
        | 'l' ->
          if
            String.unsafe_get s (pos + 1) = 'i'
            && String.unsafe_get s (pos + 2) = 'n'
            && String.unsafe_get s (pos + 3) = 'k'
          then 3
          else -1
        | _ -> -1 )
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
      | 14 ->
        if
          String.unsafe_get s pos = 'c'
          && String.unsafe_get s (pos + 1) = 'o'
          && String.unsafe_get s (pos + 2) = 'm'
          && String.unsafe_get s (pos + 3) = 'm'
          && String.unsafe_get s (pos + 4) = 'i'
          && String.unsafe_get s (pos + 5) = 't'
          && String.unsafe_get s (pos + 6) = '_'
          && String.unsafe_get s (pos + 7) = 'm'
          && String.unsafe_get s (pos + 8) = 'e'
          && String.unsafe_get s (pos + 9) = 's'
          && String.unsafe_get s (pos + 10) = 's'
          && String.unsafe_get s (pos + 11) = 'a'
          && String.unsafe_get s (pos + 12) = 'g'
          && String.unsafe_get s (pos + 13) = 'e'
        then 2
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
      field_hash := read_commit_hash p lb;
      bits0 := !bits0 lor 0x2
    | 2 ->
      field_commit_message := Atdgen_runtime.Oj_run.read_string p lb;
      bits0 := !bits0 lor 0x4
    | 3 ->
      field_link := Atdgen_runtime.Oj_run.read_string p lb;
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
        | 4 ->
          ( match String.unsafe_get s pos with
          | 'h' ->
            if
              String.unsafe_get s (pos + 1) = 'a'
              && String.unsafe_get s (pos + 2) = 's'
              && String.unsafe_get s (pos + 3) = 'h'
            then 1
            else -1
          | 'l' ->
            if
              String.unsafe_get s (pos + 1) = 'i'
              && String.unsafe_get s (pos + 2) = 'n'
              && String.unsafe_get s (pos + 3) = 'k'
            then 3
            else -1
          | _ -> -1 )
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
        | 14 ->
          if
            String.unsafe_get s pos = 'c'
            && String.unsafe_get s (pos + 1) = 'o'
            && String.unsafe_get s (pos + 2) = 'm'
            && String.unsafe_get s (pos + 3) = 'm'
            && String.unsafe_get s (pos + 4) = 'i'
            && String.unsafe_get s (pos + 5) = 't'
            && String.unsafe_get s (pos + 6) = '_'
            && String.unsafe_get s (pos + 7) = 'm'
            && String.unsafe_get s (pos + 8) = 'e'
            && String.unsafe_get s (pos + 9) = 's'
            && String.unsafe_get s (pos + 10) = 's'
            && String.unsafe_get s (pos + 11) = 'a'
            && String.unsafe_get s (pos + 12) = 'g'
            && String.unsafe_get s (pos + 13) = 'e'
          then 2
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
        field_hash := read_commit_hash p lb;
        bits0 := !bits0 lor 0x2
      | 2 ->
        field_commit_message := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x4
      | 3 ->
        field_link := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x8
      | _ -> Yojson.Safe.skip_json p lb
    done;
    assert false
  with Yojson.End_of_object ->
    if !bits0 <> 0xf then
      Atdgen_runtime.Oj_run.missing_fields p [| !bits0 |] [| "author"; "hash"; "commit_message"; "link" |];
    ( { author = !field_author; hash = !field_hash; commit_message = !field_commit_message; link = !field_link }
      : commit_pushed )

let commit_pushed_of_string s = read_commit_pushed (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write_commit : _ -> commit -> _ =
 fun ob x ->
  Bi_outbuf.add_char ob '{';
  let is_first = ref true in
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"sha\":";
  write_commit_hash ob x.sha;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"message\":";
  Yojson.Safe.write_string ob x.message;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"url\":";
  Yojson.Safe.write_string ob x.url;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"author\":";
  write_author ob x.author;
  Bi_outbuf.add_char ob '}'

let string_of_commit ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_commit ob x;
  Bi_outbuf.contents ob

let read_commit p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_lcurl p lb;
  let field_sha = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_message = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_url = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_author = ref (Obj.magic (Sys.opaque_identity 0.0)) in
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
        ( match String.unsafe_get s pos with
        | 's' -> if String.unsafe_get s (pos + 1) = 'h' && String.unsafe_get s (pos + 2) = 'a' then 0 else -1
        | 'u' -> if String.unsafe_get s (pos + 1) = 'r' && String.unsafe_get s (pos + 2) = 'l' then 2 else -1
        | _ -> -1 )
      | 6 ->
        if
          String.unsafe_get s pos = 'a'
          && String.unsafe_get s (pos + 1) = 'u'
          && String.unsafe_get s (pos + 2) = 't'
          && String.unsafe_get s (pos + 3) = 'h'
          && String.unsafe_get s (pos + 4) = 'o'
          && String.unsafe_get s (pos + 5) = 'r'
        then 3
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
      | _ -> -1
    in
    let i = Yojson.Safe.map_ident p f lb in
    Atdgen_runtime.Oj_run.read_until_field_value p lb;
    ( match i with
    | 0 ->
      field_sha := read_commit_hash p lb;
      bits0 := !bits0 lor 0x1
    | 1 ->
      field_message := Atdgen_runtime.Oj_run.read_string p lb;
      bits0 := !bits0 lor 0x2
    | 2 ->
      field_url := Atdgen_runtime.Oj_run.read_string p lb;
      bits0 := !bits0 lor 0x4
    | 3 ->
      field_author := read_author p lb;
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
        | 3 ->
          ( match String.unsafe_get s pos with
          | 's' -> if String.unsafe_get s (pos + 1) = 'h' && String.unsafe_get s (pos + 2) = 'a' then 0 else -1
          | 'u' -> if String.unsafe_get s (pos + 1) = 'r' && String.unsafe_get s (pos + 2) = 'l' then 2 else -1
          | _ -> -1 )
        | 6 ->
          if
            String.unsafe_get s pos = 'a'
            && String.unsafe_get s (pos + 1) = 'u'
            && String.unsafe_get s (pos + 2) = 't'
            && String.unsafe_get s (pos + 3) = 'h'
            && String.unsafe_get s (pos + 4) = 'o'
            && String.unsafe_get s (pos + 5) = 'r'
          then 3
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
        | _ -> -1
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      match i with
      | 0 ->
        field_sha := read_commit_hash p lb;
        bits0 := !bits0 lor 0x1
      | 1 ->
        field_message := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x2
      | 2 ->
        field_url := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x4
      | 3 ->
        field_author := read_author p lb;
        bits0 := !bits0 lor 0x8
      | _ -> Yojson.Safe.skip_json p lb
    done;
    assert false
  with Yojson.End_of_object ->
    if !bits0 <> 0xf then Atdgen_runtime.Oj_run.missing_fields p [| !bits0 |] [| "sha"; "message"; "url"; "author" |];
    ({ sha = !field_sha; message = !field_message; url = !field_url; author = !field_author } : commit)

let commit_of_string s = read_commit (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write_ci_build_status_changed : _ -> ci_build_status_changed -> _ =
 fun ob x ->
  Bi_outbuf.add_char ob '{';
  let is_first = ref true in
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"commit\":";
  write_commit ob x.commit;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"build_status\":";
  write_success_status ob x.build_status;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"branch\":";
  Yojson.Safe.write_string ob x.branch;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"link\":";
  Yojson.Safe.write_string ob x.link;
  Bi_outbuf.add_char ob '}'

let string_of_ci_build_status_changed ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_ci_build_status_changed ob x;
  Bi_outbuf.contents ob

let read_ci_build_status_changed p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_lcurl p lb;
  let field_commit = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_build_status = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_branch = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_link = ref (Obj.magic (Sys.opaque_identity 0.0)) in
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
          String.unsafe_get s pos = 'l'
          && String.unsafe_get s (pos + 1) = 'i'
          && String.unsafe_get s (pos + 2) = 'n'
          && String.unsafe_get s (pos + 3) = 'k'
        then 3
        else -1
      | 6 ->
        ( match String.unsafe_get s pos with
        | 'b' ->
          if
            String.unsafe_get s (pos + 1) = 'r'
            && String.unsafe_get s (pos + 2) = 'a'
            && String.unsafe_get s (pos + 3) = 'n'
            && String.unsafe_get s (pos + 4) = 'c'
            && String.unsafe_get s (pos + 5) = 'h'
          then 2
          else -1
        | 'c' ->
          if
            String.unsafe_get s (pos + 1) = 'o'
            && String.unsafe_get s (pos + 2) = 'm'
            && String.unsafe_get s (pos + 3) = 'm'
            && String.unsafe_get s (pos + 4) = 'i'
            && String.unsafe_get s (pos + 5) = 't'
          then 0
          else -1
        | _ -> -1 )
      | 12 ->
        if
          String.unsafe_get s pos = 'b'
          && String.unsafe_get s (pos + 1) = 'u'
          && String.unsafe_get s (pos + 2) = 'i'
          && String.unsafe_get s (pos + 3) = 'l'
          && String.unsafe_get s (pos + 4) = 'd'
          && String.unsafe_get s (pos + 5) = '_'
          && String.unsafe_get s (pos + 6) = 's'
          && String.unsafe_get s (pos + 7) = 't'
          && String.unsafe_get s (pos + 8) = 'a'
          && String.unsafe_get s (pos + 9) = 't'
          && String.unsafe_get s (pos + 10) = 'u'
          && String.unsafe_get s (pos + 11) = 's'
        then 1
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
      field_build_status := read_success_status p lb;
      bits0 := !bits0 lor 0x2
    | 2 ->
      field_branch := Atdgen_runtime.Oj_run.read_string p lb;
      bits0 := !bits0 lor 0x4
    | 3 ->
      field_link := Atdgen_runtime.Oj_run.read_string p lb;
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
        | 4 ->
          if
            String.unsafe_get s pos = 'l'
            && String.unsafe_get s (pos + 1) = 'i'
            && String.unsafe_get s (pos + 2) = 'n'
            && String.unsafe_get s (pos + 3) = 'k'
          then 3
          else -1
        | 6 ->
          ( match String.unsafe_get s pos with
          | 'b' ->
            if
              String.unsafe_get s (pos + 1) = 'r'
              && String.unsafe_get s (pos + 2) = 'a'
              && String.unsafe_get s (pos + 3) = 'n'
              && String.unsafe_get s (pos + 4) = 'c'
              && String.unsafe_get s (pos + 5) = 'h'
            then 2
            else -1
          | 'c' ->
            if
              String.unsafe_get s (pos + 1) = 'o'
              && String.unsafe_get s (pos + 2) = 'm'
              && String.unsafe_get s (pos + 3) = 'm'
              && String.unsafe_get s (pos + 4) = 'i'
              && String.unsafe_get s (pos + 5) = 't'
            then 0
            else -1
          | _ -> -1 )
        | 12 ->
          if
            String.unsafe_get s pos = 'b'
            && String.unsafe_get s (pos + 1) = 'u'
            && String.unsafe_get s (pos + 2) = 'i'
            && String.unsafe_get s (pos + 3) = 'l'
            && String.unsafe_get s (pos + 4) = 'd'
            && String.unsafe_get s (pos + 5) = '_'
            && String.unsafe_get s (pos + 6) = 's'
            && String.unsafe_get s (pos + 7) = 't'
            && String.unsafe_get s (pos + 8) = 'a'
            && String.unsafe_get s (pos + 9) = 't'
            && String.unsafe_get s (pos + 10) = 'u'
            && String.unsafe_get s (pos + 11) = 's'
          then 1
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
        field_build_status := read_success_status p lb;
        bits0 := !bits0 lor 0x2
      | 2 ->
        field_branch := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x4
      | 3 ->
        field_link := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x8
      | _ -> Yojson.Safe.skip_json p lb
    done;
    assert false
  with Yojson.End_of_object ->
    if !bits0 <> 0xf then
      Atdgen_runtime.Oj_run.missing_fields p [| !bits0 |] [| "commit"; "build_status"; "branch"; "link" |];
    ( { commit = !field_commit; build_status = !field_build_status; branch = !field_branch; link = !field_link }
      : ci_build_status_changed )

let ci_build_status_changed_of_string s =
  read_ci_build_status_changed (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
