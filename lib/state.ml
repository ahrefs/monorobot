open Base
open Common
open Devkit

type t = {
  state : State_t.state;
  lock : Lwt_mutex.t;  (** protect access to mutable string map `pipeline_statuses` *)
}

let empty_repo_state () : State_t.repo_state = { pipeline_statuses = StringMap.empty; issue_tbl = Stringtbl.empty () }
let empty_issue_state () : State_t.issue_state = { comments = StringMap.empty; reviews = StringMap.empty }

let empty () : t =
  let state = State_t.{ repos = Stringtbl.empty (); bot_user_id = None } in
  { state; lock = Lwt_mutex.create () }

let find_or_add_repo' state repo_url = Stringtbl.find_or_add state.State_t.repos repo_url ~default:empty_repo_state

let find_or_add_issue' (repo_state : State_t.repo_state) issue_num =
  Stringtbl.find_or_add repo_state.issue_tbl (Int.to_string issue_num) ~default:empty_issue_state

let set_repo_state { state; lock } repo_url repo_state =
  Lwt_mutex.with_lock lock @@ fun () ->
  Stringtbl.set state.repos ~key:repo_url ~data:repo_state;
  Lwt.return_unit

let find_or_add_repo { state; lock } repo_url =
  Lwt_mutex.with_lock lock @@ fun () -> find_or_add_repo' state repo_url |> Lwt.return

let set_repo_pipeline_status { state; lock } repo_url ~pipeline ~(branches : Github_t.branch list) ~status =
  let set_branch_status branch_statuses =
    let new_statuses = List.map branches ~f:(fun b -> b.name, status) in
    let init = Option.value branch_statuses ~default:(Map.empty (module String)) in
    List.fold_left new_statuses ~init ~f:(fun m (key, data) -> Map.set m ~key ~data)
  in
  Lwt_mutex.with_lock lock @@ fun () ->
  let repo_state = find_or_add_repo' state repo_url in
  repo_state.pipeline_statuses <- Map.update repo_state.pipeline_statuses pipeline ~f:set_branch_status;
  Lwt.return_unit

let add_comment_msg { state; lock } repo_url ~issue_num comment_id post_message_res =
  Lwt_mutex.with_lock lock @@ fun () ->
  let repo_state = find_or_add_repo' state repo_url in
  let issue_tbl = find_or_add_issue' repo_state issue_num in
  issue_tbl.comments <- Map.set issue_tbl.comments ~key:(Int.to_string comment_id) ~data:post_message_res;
  Lwt.return_unit

let get_comment_msg { state; _ } repo_url ~issue_num comment_id =
  let repo_state = find_or_add_repo' state repo_url in
  let issue_tbl = find_or_add_issue' repo_state issue_num in
  Map.find issue_tbl.comments (Int.to_string comment_id)

let add_review_msg { state; lock } repo_url ~issue_num review_id post_message_res =
  Lwt_mutex.with_lock lock @@ fun () ->
  let repo_state = find_or_add_repo' state repo_url in
  let issue_tbl = find_or_add_issue' repo_state issue_num in
  issue_tbl.reviews <- Map.set issue_tbl.reviews ~key:(Int.to_string review_id) ~data:post_message_res;
  Lwt.return_unit

let get_review_msg { state; _ } repo_url ~issue_num review_id =
  let repo_state = find_or_add_repo' state repo_url in
  let issue_tbl = find_or_add_issue' repo_state issue_num in
  Map.find issue_tbl.reviews (Int.to_string review_id)

let close_issue { state; lock } repo_url issue_num =
  Lwt_mutex.with_lock lock @@ fun () ->
  let repo_state = find_or_add_repo' state repo_url in
  Stringtbl.remove repo_state.issue_tbl issue_num;
  Lwt.return_unit

let set_bot_user_id { state; _ } user_id = state.State_t.bot_user_id <- Some user_id
let get_bot_user_id { state; _ } = state.State_t.bot_user_id
let log = Log.from "state"

let save { state; _ } path =
  let data = State_j.string_of_state state |> Yojson.Basic.from_string |> Yojson.Basic.pretty_to_string in
  match write_to_local_file ~data path with
  | Ok () -> Lwt.return @@ Ok ()
  | Error e -> Lwt.return @@ fmt_error "error while writing to local file %s: %s\nfailed to save state" path e
