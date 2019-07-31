exception ConfigError of string

module Env = struct
  let github_webhook_secret_token =
    lazy
      ( match Sys.getenv_opt "GH_WEBHOOK_SECRET_TOKEN" with
        | Some secret_token -> secret_token
        | None -> raise (ConfigError "Missing required GH_WEBHOOK_SECRET_TOKEN environment variable.") )

  let slack_webhook_url =
    lazy
      ( match Sys.getenv_opt "SLACK_WEBHOOK_URL" with
        | Some secret_token -> secret_token
        | None -> raise (ConfigError "Missing required SLACK_WEBHOOK_URL environment variable.") )
end

module Curl = struct

  let () = Curl.global_init Curl.CURLINIT_GLOBALALL

  let writer_callback a d =
    Buffer.add_string a d;
    String.length d

  let init_conn url =
    let r = Buffer.create 16384
    and c = Curl.init () in
    Curl.set_timeout c 1200;
    Curl.set_sslverifypeer c false;
    Curl.set_sslverifyhost c Curl.SSLVERIFYHOST_EXISTENCE;
    Curl.set_writefunction c (writer_callback r);
    Curl.set_tcpnodelay c true;
    Curl.set_verbose c false;
    Curl.set_url c url; 
    r,c

end
