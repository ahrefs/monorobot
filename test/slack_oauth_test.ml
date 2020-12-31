open Base
open Printf
open Lib
module Action = Action.Action (Api_local.Github) (Api_local.Slack)

let log = Devkit.Log.from "oauth_test"

let arg_code = "code", "1591663521684.1613458437648.4a18cf683e541ff9d8fd75075181cac49c7acae9431d7e4ffd424ce1ca8d2543"

let arg_state = "state", "foo"

let arg_state_bad = "state", "bar"

let secrets_default () : Config_t.secrets =
  {
    gh_token = None;
    gh_hook_token = None;
    slack_hooks = [];
    slack_access_token = None;
    slack_client_id = Some "";
    slack_client_secret = Some "";
    slack_signing_secret = None;
    slack_oauth_state = None;
  }

let secrets_with_state = { (secrets_default ()) with slack_oauth_state = Some "foo" }

let secrets_with_token = { (secrets_default ()) with slack_access_token = Some "original token" }

let tests =
  [
    "success, valid params", [ arg_code ], secrets_default (), Some (sprintf "token of code %s" (snd arg_code));
    "success, with state", [ arg_code; arg_state ], secrets_with_state, Some (sprintf "token of code %s" (snd arg_code));
    "no-op, token exists", [ arg_code ], secrets_with_token, Some "original token";
    "failure, no state arg", [ arg_code ], secrets_with_state, None;
    "failure, bad state arg", [ arg_code; arg_state_bad ], secrets_with_state, None;
    "failure, no code arg", [], secrets_default (), None;
  ]

let process (name, args, secrets, expected_token) =
  let ctx = Context.make () in
  ctx.secrets <- Some secrets;
  Option.iter secrets.slack_access_token ~f:(fun t -> ctx.state.slack_access_token <- Some t);
  let%lwt _msg = Action.process_slack_oauth ctx args in
  match assert ((Option.equal String.equal) ctx.state.slack_access_token expected_token) with
  | exception Assert_failure _ -> Lwt.return @@ Error (sprintf "test failed: %s" name)
  | () -> Lwt.return @@ Ok ()

let () =
  Lwt_main.run
    ( match%lwt Lwt_list.map_s process tests |> Lwt.map Result.combine_errors_unit with
    | Ok () -> Lwt.return_unit
    | Error e ->
      List.iter e ~f:Stdio.print_endline;
      Caml.exit 1
    )
