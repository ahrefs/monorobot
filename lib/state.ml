open Base
open Common
open Devkit

type t = {
  state : State_t.state;
  lock : Lwt_mutex.t;  (** protect access to mutable string map `pipeline_statuses` *)
}

let empty_repo_state () : State_t.repo_state =
  { pipeline_statuses = StringMap.empty; comments_map = StringMap.empty; reviews_map = StringMap.empty }

let empty () : t =
  let state = State_t.{ repos = Stringtbl.empty (); bot_user_id = None } in
  { state; lock = Lwt_mutex.create () }

let find_or_add_repo' state repo_url = Stringtbl.find_or_add state.State_t.repos repo_url ~default:empty_repo_state

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

let add_comment_map { state; lock } repo_url comment_id post_message_res =
  Lwt_mutex.with_lock lock @@ fun () ->
  let repo_state = find_or_add_repo' state repo_url in
  repo_state.comments_map <- Map.set repo_state.comments_map ~key:(Int.to_string comment_id) ~data:post_message_res;
  Lwt.return_unit

let get_comment_map { state; _ } repo_url comment_id =
  let repo_state = find_or_add_repo' state repo_url in
  Map.find repo_state.comments_map (Int.to_string comment_id)

let add_review_map { state; lock } repo_url review_id post_message_res =
  Lwt_mutex.with_lock lock @@ fun () ->
  let repo_state = find_or_add_repo' state repo_url in
  repo_state.reviews_map <- Map.set repo_state.reviews_map ~key:(Int.to_string review_id) ~data:post_message_res;
  Lwt.return_unit

let get_review_map { state; _ } repo_url review_id =
  let repo_state = find_or_add_repo' state repo_url in
  Map.find repo_state.reviews_map (Int.to_string review_id)

let set_bot_user_id { state; _ } user_id = state.State_t.bot_user_id <- Some user_id
let get_bot_user_id { state; _ } = state.State_t.bot_user_id
let log = Log.from "state"

let save { state; _ } path =
  let data = State_j.string_of_state state |> Yojson.Basic.from_string |> Yojson.Basic.pretty_to_string in
  match write_to_local_file ~data path with
  | Ok () -> Lwt.return @@ Ok ()
  | Error e -> Lwt.return @@ fmt_error "error while writing to local file %s: %s\nfailed to save state" path e
