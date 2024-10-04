open Github_t
open Slack_t

module type Github = sig
  val get_config : ctx:Context.t -> repo:repository -> (Config_t.config, string) Result.t Lwt.t
  val get_api_commit : ctx:Context.t -> repo:repository -> sha:string -> (api_commit, string) Result.t Lwt.t
  val get_pull_request : ctx:Context.t -> repo:repository -> number:int -> (pull_request, string) Result.t Lwt.t
  val get_issue : ctx:Context.t -> repo:repository -> number:int -> (issue, string) Result.t Lwt.t
  val get_compare : ctx:Context.t -> repo:repository -> basehead:Github.basehead -> (compare, string) Result.t Lwt.t

  val request_reviewers :
    ctx:Context.t -> repo:repository -> number:int -> reviewers:request_reviewers_req -> (unit, string) Result.t Lwt.t
end

module type Slack = sig
  val lookup_user :
    ?cache:[ `Use | `Refresh ] ->
    ctx:Context.t ->
    cfg:Config_t.config ->
    email:string ->
    unit ->
    lookup_user_res slack_response Lwt.t

  val list_users : ?cursor:string -> ?limit:int -> ctx:Context.t -> unit -> list_users_res slack_response Lwt.t
  val send_notification : ctx:Context.t -> msg:post_message_req -> post_message_res option slack_response Lwt.t

  val send_chat_unfurl :
    ctx:Context.t ->
    channel:string ->
    ts:string ->
    unfurls:message_attachment Common.StringMap.t ->
    unit ->
    unit slack_response Lwt.t

  val send_auth_test : ctx:Context.t -> unit -> auth_test_res slack_response Lwt.t
end
