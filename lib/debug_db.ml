open Printf

let replay_action db_path pipeline branch only_with_changes after state build_number step_name sha =
  Lwt_main.run
    (let db_path = Option.default "db/monorobot.db" db_path in
     let%lwt () = Database.init db_path in
     match pipeline, branch with
     | None, _ | _, None -> failwith "pipeline and branch are required"
     | Some pipeline, (Some branch : string option) ->
       (* [<string>%%] is a wildcard to look for any string that starts with [<string>] *)
       let pipeline = sprintf "%s%%" pipeline in
       let q, v =
         let open Database.Debug_db in
         let args =
           [
             "after", Option.map string_of_int after;
             "build_number", Option.map string_of_int build_number;
             "sha", sha;
             "step_name", step_name;
           ]
         in
         match List.filter (fun (_, arg) -> Option.is_some arg) args with
         | _ :: _ :: _ ->
           failwith "too many options: the after, build_number, sha and step_name options cannot be used together"
         | [] ->
           failwith
             "no options provided. Please provide a search criteria, like --after, --build-number, --sha, --step-name"
         | [ ("after", Some after) ] -> get_by_range ~branch ~pipeline, after
         | [ ("build_number", Some build_number) ] -> get_by_build_number ~branch ~pipeline, build_number
         | [ ("sha", Some sha) ] -> get_by_sha ~branch ~pipeline, sha
         | [ ("step_name", Some step_name) ] -> get_by_step_name ~branch ~pipeline, step_name
         | _ -> failwith "unexpected options"
       in
       let fold ~id ~last_handled_in ~description ~notification_text ~n_state ~has_state_update
         ~state_before_notification ~state_after_notification acc =
         let row_log =
           let n = notification_text |> Debug_db_j.status_notification_of_string in
           let branches_str =
             sprintf "[%s]" @@ String.concat ", " (List.map (fun (b : Github_t.branch) -> b.name) n.branches)
           in
           let print_commit_hash s = String.sub s 0 (min 8 @@ String.length s) in
           let state_diff =
             match has_state_update with
             | false -> "\n"
             | true ->
               let json s = Yojson.Basic.(from_string s |> pretty_to_string) in
               sprintf "# STATE DIFF:\n\n%s\n"
                 Odiff.(
                   strings_diffs (json state_before_notification) (json state_after_notification) |> string_of_diffs)
           in
           let header =
             sprintf "[%s] event status: commit=%s, state=%s, context=%s, target_url=%s, branches=%s" n.name
               (print_commit_hash n.sha)
               (Github_j.string_of_status_state n.state)
               n.context (Option.default "none" n.target_url) branches_str
           in
           sprintf {|%s

# ID: %Ld
# DESCRIPTION: %s
# LAST HANDLED IN: %s
# HAS STATE UPDATE: %s
%s|} header id
             description last_handled_in (string_of_bool has_state_update) state_diff
         in
         let keep_row =
           let with_changes_filter =
             match only_with_changes with
             | true when has_state_update -> true
             | true -> false
             | false -> true
           in
           let state_filter =
             match state with
             | None -> true
             | Some state -> state = Github_j.status_state_of_string n_state
           in
           with_changes_filter && state_filter
         in
         match keep_row with
         | false -> acc
         | true -> sprintf "%s\n%s" row_log acc
       in
       (match%lwt q v fold with
       | Ok log ->
         print_endline log;
         Lwt.return_unit
       | Error e -> failwith e
       | Db_unavailable -> failwith "database unavailable"))
