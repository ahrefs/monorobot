exception ConfigError of string

module Env = struct
  let github_sha1_signature = match Sys.getenv_opt "SHA1_SIG" with
    | Some sha1_signature -> sha1_signature
    | None -> raise (ConfigError "Missing required SHA1_SIG environment variable.")

  let github_user_agent = match Sys.getenv_opt "GITHUB_AGENT" with
    | Some user_agent  -> user_agent
    | None -> raise (ConfigError "Missing required GITHUB_AGENT environment variable.")
end
