exception Context_Error of string

type cfg_sources =
  | Local
  | Remote

type data = {
  mutable cfg_path : string;
  cfg_source : cfg_sources;
  cfg_action_after_refresh : Config.t -> unit;
  secrets_path : string;
  state_path : string;
  disable_write : bool;
}

type t = {
  mutable state : Notabot_t.state;
  mutable cfg : Config.t;
  secrets : Notabot_t.secrets;
  data : data;
}

type context_thunk = {
  secrets : Notabot_j.secrets;
  thunk : ?req:Github.t -> unit -> t Lwt.t;
  mutable ctx : t option;
}

val refresh_config : t -> unit Lwt.t

val refresh_and_get_config : t -> Config.t Lwt.t

val change_remote_url : t -> Github.t -> unit Lwt.t

val refresh_state : t -> unit

val refresh_and_get_state : t -> Notabot_t.state

val update_state : t -> Github.t -> unit

val update_and_get_state : t -> Github.t -> Notabot_t.state

val make_with_secrets
  :  state_path:string ->
  ?cfg_path:string ->
  secrets_path:string ->
  secrets:Notabot_t.secrets ->
  ?disable_write:bool ->
  ?cfg_action_after_refresh:(Config.t -> unit) ->
  ?req:Github.t ->
  unit ->
  t Lwt.t

val make
  :  state_path:string ->
  ?cfg_path:string ->
  secrets_path:string ->
  ?disable_write:bool ->
  ?cfg_action_after_refresh:(Config.t -> unit) ->
  ?req:Github.t ->
  unit ->
  t Lwt.t

val make_thunk
  :  state_path:string ->
  ?cfg_path:string ->
  secrets_path:string ->
  ?disable_write:bool ->
  ?cfg_action_after_refresh:(Config.t -> unit) ->
  unit ->
  context_thunk

val resolve_ctx_in_thunk : context_thunk -> Github.t -> t Lwt.t
