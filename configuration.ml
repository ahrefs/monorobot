exception ConfigError of string

module Env = struct
  let github_webhook_secret_token =
    lazy
      ( match Sys.getenv_opt "GH_WEBHOOK_SECRET_TOKEN" with
      | Some secret_token -> secret_token
      | None -> raise (ConfigError "Missing required GH_WEBHOOK_SECRET_TOKEN environment variable.") )
end
