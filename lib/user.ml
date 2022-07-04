open Devkit

let log = Log.from "user"

type t = {
  github : unit;
  slack : Slack_t.user;
}

let load_all ~ctx () =
  match Context.list_repo_configs ctx with
  | [] -> log#info "no registered"
  | repos ->
  match%lwt Api_remote.Slack.users_list ~ctx () with
  | Error e -> log#error "User.load_all: %s" e; Lwt.return_unit
  | Ok slack_users ->
    let repos 
    let slack_users = List.filter (Slack.is_active_human_user) slack_users.members in


