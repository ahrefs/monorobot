exception ConfigError of string

module Env = struct
  let github_sha1_signature = Sys.getenv_opt "SHA1_SIG"

  let github_user_agent = Sys.getenv_opt "GITHUB_AGENT"

  let ensure_env name =
    match Sys.getenv_opt name with
    | Some _ -> ()
    | None -> raise (ConfigError (Printf.sprintf "Missing required environment variable: %s." name))
end
