(** Extract blockquote lines and remaining body from a Slack message.
    Lines starting with ">" are blockquote lines. Returns (quoted_text, body_text).
    The ">" prefix and leading space are stripped from quoted lines. *)
let extract_blockquote text =
  let lines = String.split_on_char '\n' text in
  let quoted, body =
    List.partition_map
      (fun line ->
        let trimmed = String.trim line in
        match String.length trimmed > 0 && trimmed.[0] = '>' with
        | true ->
          let content = String.sub trimmed 1 (String.length trimmed - 1) in
          let content =
            match String.length content > 0 && content.[0] = ' ' with
            | true -> String.sub content 1 (String.length content - 1)
            | false -> content
          in
          Left content
        | false -> Right line)
      lines
  in
  let quoted_text = String.concat "\n" quoted in
  let body_text = String.concat "\n" body |> String.trim in
  quoted_text, body_text

(** Convert Slack user mentions <@U12345> to @github_login using a resolver function.
    The resolver takes a Slack user ID string and returns an optional GitHub login. *)
let convert_user_mentions ~resolve_user text =
  Re2.replace_exn (Re2.create_exn "<@(U[A-Z0-9]+)>") text ~f:(fun match_ ->
      let slack_id = Re2.Match.get_exn ~sub:(`Index 1) match_ in
      match resolve_user slack_id with
      | Some github_login -> Printf.sprintf "@%s" github_login
      | None -> Printf.sprintf "@%s" slack_id)

(** Convert Slack channel mentions <#C12345|channel-name> to #channel-name *)
let convert_channel_mentions text =
  Re2.replace_exn (Re2.create_exn "<#C[A-Z0-9]+\\|([^>]+)>") text ~f:(fun match_ ->
      let name = Re2.Match.get_exn ~sub:(`Index 1) match_ in
      Printf.sprintf "#%s" name)

(** Convert Slack links <url|text> to [text](url) and bare <url> to url *)
let convert_links text =
  let text =
    Re2.replace_exn (Re2.create_exn "<(https?://[^|>]+)\\|([^>]+)>") text ~f:(fun match_ ->
        let url = Re2.Match.get_exn ~sub:(`Index 1) match_ in
        let label = Re2.Match.get_exn ~sub:(`Index 2) match_ in
        Printf.sprintf "[%s](%s)" label url)
  in
  Re2.replace_exn (Re2.create_exn "<(https?://[^>]+)>") text ~f:(fun match_ -> Re2.Match.get_exn ~sub:(`Index 1) match_)

(** Convert Slack bold *text* to GitHub bold **text**
    Be careful not to double-convert already-double-starred text *)
let convert_bold text =
  (* Match single asterisks that are not already doubled.
     Re2 doesn't support lookbehinds, so use a two-pass approach:
     first protect existing **, then convert single *, then restore ** *)
  let placeholder = "\x00DBLSTAR\x00" in
  let text = Re2.replace_exn (Re2.create_exn {|\*\*|}) text ~f:(fun _ -> placeholder) in
  let text =
    Re2.replace_exn (Re2.create_exn {|\*([^*]+)\*|}) text ~f:(fun match_ ->
        let content = Re2.Match.get_exn ~sub:(`Index 1) match_ in
        Printf.sprintf "**%s**" content)
  in
  Re2.replace_exn (Re2.create_exn (Re2.escape placeholder)) text ~f:(fun _ -> "**")

(** Full conversion from Slack mrkdwn to GitHub markdown.
    resolve_user: takes a Slack user ID string, returns optional GitHub login *)
let to_github_markdown ~resolve_user text =
  text |> convert_user_mentions ~resolve_user |> convert_channel_mentions |> convert_links |> convert_bold
