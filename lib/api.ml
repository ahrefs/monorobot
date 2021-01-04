open Base
open Github_t
open Slack_t

module type Github = sig
  val get_config : ctx:Context.t -> repo:repository -> (Config_t.config, string) Result.t Lwt.t

  val get_api_commit : ctx:Context.t -> repo:repository -> sha:string -> (api_commit, string) Result.t Lwt.t

  val get_pull_request : ctx:Context.t -> repo:repository -> number:int -> (pull_request, string) Result.t Lwt.t

  val get_issue : ctx:Context.t -> repo:repository -> number:int -> (issue, string) Result.t Lwt.t
end

module type Slack = sig
  val send_notification : ctx:Context.t -> msg:post_message_req -> (unit, string) Result.t Lwt.t

  val send_chat_unfurl : ctx:Context.t -> chat_unfurl_req -> (unit, string) Result.t Lwt.t
end
