open Devkit
open Common

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
      match Stre.exists build_url (context ^ "/") with
      (* We need to add the "/" to context to avoid partial matches between the context and the build_url *)
      | true -> Some context
      | false ->
      (* Matches the notification context against the build_url to get the base pipeline name.
         Drop path levels from the context until we find a match with the build_url. This is the base name. *)
      match String.rindex_opt context '/' with
      | Some idx -> get_name (String.sub context 0 idx) build_url
      | None -> None
    in
    (* if we have a buildkite pipeline, we need to strip the `buildkite/` prefix to get the real name *)
    let context' = Stre.drop_prefix context "buildkite/" in
    let pipeline_name = get_name context' build_url in
    Option.map (fun pipeline_name -> { is_pipeline_step = pipeline_name <> context'; pipeline_name }) pipeline_name

  let parse_context_exn ~context ~build_url =
    match parse_context ~context ~build_url with
    | Some c -> c
    | None ->
      failwith
        (Printf.sprintf "failed to get pipeline name from notification. Context: %s, Build URL: %s" context build_url)

  let get_build_number_exn ~context ~build_url =
    let { pipeline_name; _ } = parse_context_exn ~context ~build_url in
    (* build urls are in the form of .../<base_pipeline_name>/builds/<build_number>.
       Pipeline steps have an html anchor after the build number *)
    let re = Re2.create_exn (Printf.sprintf ".*/%s/builds/(\\d+)#?" pipeline_name) in
    match Re2.find_first_exn ~sub:(`Index 1) re build_url with
    | build_number -> build_number
    | exception _ -> failwith "failed to get build number from url"

  let is_failed_build (n : Github_t.status_notification) =
    n.state = Failure && Re2.matches buildkite_is_failed_re (Option.default "" n.description)

  let new_failed_steps (n : Github_t.status_notification) (repo_state : State_t.repo_state) =
    if not (is_failed_build n) then failwith "Error: new_failed_steps fn must be called on a finished build";
    match n.target_url with
    | None ->
      (* if we don't have a target_url value, we don't have a build number and cant't track build state *)
      []
    | Some build_url ->
      let { pipeline_name; _ } = parse_context_exn ~context:n.context ~build_url in
      (match n.state = Failure, n.branches with
      | false, _ -> []
      | true, [ branch ] ->
        (match StringMap.find_opt pipeline_name repo_state.pipeline_statuses with
        | Some branches_statuses ->
          (match StringMap.find_opt branch.name branches_statuses with
          | Some builds_maps ->
            let current_build_number = get_build_number_exn ~context:n.context ~build_url in
            let to_previous_failed_steps n build_number (build_status : State_t.build_status) acc =
              match int_of_string build_number >= n with
              | true -> acc
              | false -> build_status.failed_steps @ acc
            in
            let previous_failed_steps =
              StringMap.fold (to_previous_failed_steps @@ int_of_string current_build_number) builds_maps []
              |> List.sort_uniq Stdlib.compare
            in
            let current_build =
              (* We will not get an exception here because we checked that the build is failed and finished *)
              StringMap.find current_build_number builds_maps
            in
            List.filter
              (fun (step : State_t.failed_step) ->
                not @@ List.exists (fun (prev : State_t.failed_step) -> prev.name = step.name) previous_failed_steps)
              current_build.failed_steps
          | None -> [])
        | None -> [])
      | true, _ -> [])
end
