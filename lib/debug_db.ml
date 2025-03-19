open Printf

let replay_action db_path pipeline branch only_with_changes after state build_number sha from to_ =
  Lwt_main.run
    (let db_path = Option.default "db/monorobot.db" db_path in
     let%lwt () = Database.init db_path in
     match pipeline, branch with
     | None, _ | _, None -> failwith "pipeline and branch are required"
     | Some pipeline, Some branch ->
       let q, v =
         let open Database.Debug_db in
         let args =
           [
             "after", Option.map string_of_int after;
             "build_number", Option.map string_of_int build_number;
             "sha", sha;
             "from", Option.map string_of_int from;
             "to", Option.map string_of_int to_;
           ]
         in
         match List.filter (fun (_, arg) -> Option.is_some arg) args with
         | [] -> failwith "no options provided. Please provide a search criteria, like --after, --build-number, --sha"
         | [ ("after", Some after) ] -> get_after ~branch ~pipeline, after
         | [ ("build_number", Some build_number) ] -> get_by_build_number ~branch ~pipeline, build_number
         | [ ("sha", Some sha) ] -> get_by_sha ~branch ~pipeline, sha
         | [ ("from", Some from); ("to", Some to_) ] | [ ("to", Some to_); ("from", Some from) ] ->
           get_from_to ~branch ~pipeline ~from ~to_, ""
         | _ :: _ :: _ ->
           failwith "too many options: the after, build_number, sha and step_name options cannot be used together"
         | _ -> failwith "unexpected options"
       in
       let fold ~id ~sha ~build_state ~build_number ~is_canceled ~has_state_update ~last_handled_in
         ~state_before_notification ~state_after_notification acc =
         let row_log =
           let print_commit_hash s = String.sub s 0 (min 8 @@ String.length s) in
           let state_diff =
             match has_state_update with
             | false -> "\n"
             | true ->
               let json = function
                 | "" -> ""
                 | s -> Yojson.Basic.(from_string s |> pretty_to_string)
               in
               sprintf "# STATE DIFF:\n\n%s\n"
                 Odiff.(
                   strings_diffs (json state_before_notification) (json state_after_notification) |> string_of_diffs)
           in
           let header =
             sprintf
               "==============================================================\n\
                [%s/%Ld] branch=%s, commit=%s, state=%s, canceled=%b, db_id=%s" pipeline build_number branch
               (print_commit_hash sha) build_state is_canceled id
           in
           let build_state' =
             match Buildkite_j.build_state_of_string build_state with
             | Passed when has_state_update -> "FIXED"
             | Passed -> "PASSED"
             | Failed -> "FAILED"
             | Canceled -> "CANCELED"
             | _ -> "UNKNOWNED: " ^ build_state
           in
           sprintf {|%s

# BUILD NUMBER: %Ld
# BUILD STATE: %s
# LAST HANDLED IN: %s
# HAS STATE UPDATE: %b
%s|} header
             build_number build_state' last_handled_in has_state_update state_diff
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
             | Some state -> state = Buildkite_j.build_state_of_string build_state
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
