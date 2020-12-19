open Base
open Github_t
open Slack_t

module type Github = sig
  val get_config : ctx:Context.t -> repo:repository -> (Config_t.config, string) Result.t Lwt.t

  val get_api_commit : ctx:Context.t -> repo:repository -> sha:string -> (api_commit, string) Result.t Lwt.t
end

module type Slack = sig
  val send_notification : chan:string -> msg:webhook_notification -> url:string -> (unit, string) Result.t Lwt.t
end
