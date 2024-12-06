open Devkit

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
  type pipeline_check = {
    is_pipeline_step : bool;
    pipeline_name : string;
  }

  let buildkite_is_failed_re = Re2.create_exn {|^Build #\d+ failed|}

  let parse_context ~context ~build_url =
    let rec get_name context build_url =
      match String.length context with
      | 0 -> Error "failed to get pipeline name from notification"
      | _ when Devkit.Stre.exists build_url (context ^ "/") ->
        (* We need to add the "/" to context to avoid partial matches between the context and the build_url *)
        Ok context
      | _ ->
      (* Matches the notification context against the build_url to get the base pipeline name.
         Drop path levels from the context until we find a match with the build_url. This is the base name. *)
      match String.rindex_opt context '/' with
      | Some idx -> get_name (String.sub context 0 idx) build_url
      | None -> Error "failed to get pipeline name from notification"
    in
    (* if we have a buildkit pipeline, we need to strip the `buildkite/` prefix to get the real name *)
    let context' = Stre.drop_prefix context "buildkite/" in
    let pipeline_name = get_name context' build_url in
    Result.map (fun pipeline_name -> { is_pipeline_step = pipeline_name <> context'; pipeline_name }) pipeline_name

  let get_build_number ~context ~build_url =
    match parse_context ~context ~build_url with
    | Error msg -> Error msg
    | Ok { pipeline_name; _ } ->
      (* build urls are in the form of .../<base_pipeline_name>/builds/<build_number>.
         Pipeline steps have an html anchor after the build number *)
      let re = Re2.create_exn (Printf.sprintf ".*/%s/builds/(\\d+)#?" pipeline_name) in
      (match Re2.find_submatches_exn re build_url with
      | [| _; Some build_number |] -> Ok build_number
      | _ | (exception _) ->
        (* we either match on the first case or get an exception. *)
        Error "failed to get build number from url")

  let is_failed_build (n : Github_t.status_notification) =
    n.state = Failure && Re2.matches buildkite_is_failed_re (Option.default "" n.description)

  let new_failed_steps (n : Github_t.status_notification) (repo_state : State_t.repo_state) pipeline =
    let to_failed_steps branch step statuses acc =
      (* check if step of an allowed pipeline *)
      match step with
      | step when step = pipeline -> acc
      | step when not @@ Devkit.Stre.starts_with step pipeline -> acc
      | _ ->
      match Common.StringMap.find_opt branch statuses with
      | Some (s : State_t.build_status) when s.status = Failure ->
        (match s.current_failed_commit, s.original_failed_commit with
        | Some _, _ ->
          (* if we have a value for current_failed_commit, this step was already failed and notified *)
          acc
        | None, Some { build_link = Some build_link; sha; _ } when sha = n.commit.sha ->
          (* we need to check the value of the commit sha to avoid false positives *)
          (step, build_link) :: acc
        | _ -> acc)
      | _ -> acc
    in
    match n.state = Failure, n.branches with
    | false, _ -> []
    | true, [ branch ] -> Common.StringMap.fold (to_failed_steps branch.name) repo_state.pipeline_statuses []
    | true, _ -> []
end
