open Devkit

module StringMap = Common.StringMap

let fmt_error ?exn fmt =
  Printf.ksprintf
    (fun s ->
      match exn with
      | Some exn -> Error (s ^ " :  exn " ^ Exn.str exn)
      | None -> Error s)
    fmt

let first_line s =
  match String.split_on_char '\n' s with
  | x :: _ -> x
  | [] -> s

let decode_string_pad s = Stre.rstrip ~chars:"= \n\r\t" s |> Base64.decode_exn ~pad:false

let http_request ?headers ?body meth path =
  let setup h =
    Curl.set_followlocation h true;
    Curl.set_maxredirs h 1
  in
  match%lwt Web.http_request_lwt ~setup ~ua:"monorobot" ~verbose:true ?headers ?body meth path with
  | `Ok s -> Lwt.return @@ Ok s
  | `Error e -> Lwt.return @@ Error e

let sign_string_sha256 ~key ~basestring =
  Cstruct.of_string basestring |> Nocrypto.Hash.SHA256.hmac ~key:(Cstruct.of_string key) |> Hex.of_cstruct |> Hex.show

module Build = struct
  let new_failed_steps (n : Github_t.status_notification) (repo_state : State_t.repo_state) pipeline =
    let to_failed_steps branch step statuses acc =
      (* check if step of an allowed pipeline *)
      match step with
      | step when step = pipeline -> acc
      | step when not @@ Devkit.Stre.starts_with step pipeline -> acc
      | _ ->
      match StringMap.find_opt branch statuses with
      | Some (s : State_t.build_status) when s.status = Failure ->
        (match s.current_failed_commit, s.original_failed_commit with
        | Some _, _ ->
          (* if we have a value for current_failed_commit, this step was already failed and notified *)
          acc
        | None, Some { build_link = Some build_link; sha; url; _ } when sha = n.commit.sha ->
          (* we need to check the value of the commit sha to avoid false positives *)
          (step, (build_link, url)) :: acc
        | _ -> acc)
      | _ -> acc
    in
    match n.state = Failure, n.branches with
    | false, _ -> []
    | true, [ branch ] -> StringMap.fold (to_failed_steps branch.name) repo_state.pipeline_statuses []
    | true, _ -> []

  let all_failed_steps (n : Github_t.status_notification) (repo_state : State_t.repo_state) pipeline =
    try
      (* notification.branches should always have _at least_ (and at most) one element,
         but we wrap this block in a try/with to make sure we don't raise from List.hd *)
      let branch = List.hd n.branches in
      StringMap.fold
        (fun step statuses acc ->
          (* check if step of an allowed pipeline *)
          match step with
          | step when step = pipeline -> acc
          | step when not @@ Devkit.Stre.starts_with step pipeline -> acc
          | _ ->
          match StringMap.find_opt branch.name statuses with
          | Some (s : State_t.build_status) when s.status = Failure ->
            (match s.original_failed_commit with
            | Some { build_link = Some build_link; url; _ } -> (step, (build_link, url)) :: acc
            | _ -> acc)
          | _ -> acc)
        repo_state.pipeline_statuses []
    with _e -> []
end

module Slack = struct
  let slack_step_link pipeline (s, (l, url)) =
    let step = Devkit.Stre.drop_prefix s (pipeline ^ "/") in
    Printf.sprintf "<%s|%s> (<%s|gh>)" l step url
end
