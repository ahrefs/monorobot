type db_status =
  | Not_available
  | Available

type 'a db_use_result =
  | Ok of 'a
  | Error of string
  | Db_unavailable

let log = Devkit.Log.from "database"

let db_path = "db/monorobot.db"

let available = ref Not_available

module Conn = struct
  include Sqlgg_sqlite3

  module IO = struct
    type 'a future = 'a Lwt.t

    let return = Lwt.return

    let ( >>= ) = Lwt.bind

    let bracket x dtor f =
      let%lwt x = x in
      (f x) [%finally dtor x]
  end

  let finish_params params = Lwt.wrap1 finish_params params

  let no_params params = Lwt.wrap1 no_params params

  let unsafe_wrap set_params stmt = ignore @@ set_params stmt

  let select db sql (set_params : statement -> result IO.future) callback =
    Lwt.wrap4 select db sql (unsafe_wrap set_params) callback

  let select_one db sql (set_params : statement -> result IO.future) callback =
    Lwt.wrap4 select_one db sql (unsafe_wrap set_params) callback

  let select_one_maybe db sql (set_params : statement -> result IO.future) callback =
    Lwt.wrap4 select_one_maybe db sql (unsafe_wrap set_params) callback

  let execute db sql (set_params : statement -> result IO.future) = Lwt.wrap3 execute db sql (unsafe_wrap set_params)

  module Pool : sig
    type connection = Sqlite3.db

    type t

    val create : max_conn:int -> string -> t Lwt.t

    val use : t -> (connection -> 'a Lwt.t) -> 'a Lwt.t

    val shutdown : t -> unit Lwt.t
  end = struct
    type connection = Sqlite3.db

    type t = {
      pool : connection Lwt_pool.t;
      connections : connection list;
    }

    let create ~max_conn db_path =
      let connections = ref [] in

      let create_connection () =
        let conn = Sqlite3.db_open db_path in
        connections := conn :: !connections;
        Lwt.return conn
      in

      let dispose_connection conn =
        connections := List.filter (( != ) conn) !connections;
        Lwt.return (ignore (Sqlite3.db_close conn))
      in

      let validate_connection (conn : connection) : bool Lwt.t =
        try
          let r = Sqlite3.exec conn "SELECT 1" ~cb:(fun _ _ -> ()) in
          Lwt.return (r = Sqlite3.Rc.OK)
        with _ -> Lwt.return false
      in

      Lwt.return
        {
          pool = Lwt_pool.create max_conn ~validate:validate_connection ~dispose:dispose_connection create_connection;
          connections = !connections;
        }

    let use t f = Lwt_pool.use t.pool f

    let shutdown t = Lwt_list.iter_p (fun conn -> Lwt.return (ignore (Sqlite3.db_close conn))) t.connections
  end
end

type connection = Conn.Pool.connection

let pool : Conn.Pool.t option ref = ref None

let with_db f_name (f : connection -> 'a Lwt.t) : 'a db_use_result Lwt.t =
  match !available with
  | Not_available -> Lwt.return Db_unavailable
  | Available ->
    let pool = Option.get !pool in
    (match%lwt Conn.Pool.use pool f with
    | x -> Lwt.return (Ok x)
    | exception e ->
      let exn_str = Printexc.to_string e in
      log#error "[%s] %s" f_name exn_str;
      Lwt.return (Error exn_str))

module Failed_builds = struct
  module T = Failed_builds_webhook_gen.Make (Conn)

  type n = Buildkite_t.webhook_build_payload

  let create ~(ctx : Context.t)
    ({ build = { id; sha; meta_data; state; web_url; number; branch; created_at; _ }; pipeline; _ } as n : n) =
    let op_name = "Failed_builds.create" in
    let handle_create r =
      let repo_slug = n.pipeline.provider.settings.repository in
      match%lwt r with
      | Ok _ ->
        log#debug "[%s] [%s] created failed build" op_name repo_slug;
        Lwt.return_unit
      | Error e ->
        log#error "[%s] [%s] failed to create failed build: %s" op_name repo_slug e;
        Lwt.return_unit
      | Db_unavailable ->
        log#debug "[%s] [%s] database unavailable" op_name repo_slug;
        Lwt.return_unit
    in
    handle_create
    @@ with_db op_name (fun dbd ->
           let org, pipeline_name, _build_nr = Util.Build.get_org_pipeline_build' web_url in
           let repo_url = Util.Build.git_ssh_to_https pipeline.repository in
           let repo_state = State.find_or_add_repo ctx.state repo_url in
           let%lwt (build : Buildkite_t.get_build_res) =
             (* TODO: review. Can we `Use cache here? *)
             let%lwt b = Api_remote.Buildkite.get_build ~cache:`Refresh ~ctx web_url in
             Lwt.return @@ Result.get_ok b
           in
           let jobs =
             List.filter
               (function
                 | Buildkite_t.Script _ | Buildkite_t.Trigger _ -> true
                 | _ -> false)
               build.jobs
           in
           let build_payload = Buildkite_j.string_of_get_build_res { build with jobs } in
           let commit_author = Util.Webhook.extract_metadata_email meta_data |> Option.default "" in
           let commit_url = Printf.sprintf "%s/commit/%s" repo_url sha in
           let notification_created_at = Common.Timestamp.wrap_with_fallback created_at |> Ptime.to_float_s in
           let state_before_notification =
             match Common.Stringtbl.find_opt repo_state.failed_steps (Util.Webhook.repo_key org pipeline_name) with
             | Some state -> State_j.string_of_failed_steps state
             | None -> ""
           in

           (* These values should/will be updated using the update_state_after_notification function. *)
           let state_after_notification = "" in
           let has_state_update = false in

           T.create ~id ~sha ~build_payload
             ~pipeline_payload:(Buildkite_j.string_of_pipeline pipeline)
             ~jobs:(Buildkite_j.string_of_jobs jobs) ~commit_author ~commit_url
             ~build_state:(Buildkite_j.string_of_build_state state)
             ~build_url:web_url ~build_number:(Int64.of_int number) ~is_canceled:(state = Canceled)
             ~pipeline:(Util.Webhook.repo_key org pipeline_name)
             ~repository:repo_url ~branch ~state_before_notification ~state_after_notification ~has_state_update
             ~notification_created_at
             ~created_at:(Ptime_clock.now () |> Ptime.to_float_s)
             ~last_handled_in:"create" dbd)

  let update_state_after_notification ~(repo_state : State_t.repo_state) ~has_state_update (n : n) last_handled_in =
    let org, pipeline_name, _build_nr = Util.Build.get_org_pipeline_build' n.build.web_url in
    let state_after_notification =
      match Common.Stringtbl.find_opt repo_state.failed_steps (Util.Webhook.repo_key org pipeline_name) with
      | Some state -> State_j.string_of_failed_steps state
      | None -> "no state found to write"
    in
    let op_name = "Failed_builds.update_state_after_notification" in
    let handle_update r =
      let repo_slug = n.pipeline.provider.settings.repository in
      match%lwt r with
      | Ok _ ->
        log#debug "[%s] [%s] updated state after notification" op_name repo_slug;
        Lwt.return_unit
      | Error e ->
        log#error "[%s] [%s] failed to update state after notification: %s" op_name repo_slug e;
        Lwt.return_unit
      | Db_unavailable ->
        log#debug "[%s] [%s] database unavailable" op_name repo_slug;
        Lwt.return_unit
    in
    handle_update
    @@ with_db
         (Printf.sprintf "Failed_builds.update_state_after_notification %s" last_handled_in)
         (T.update_state_after_notification ~id:n.build.id ~has_state_update ~state_after_notification ~last_handled_in)
end

module Debug_db = struct
  module Replay = Failed_builds_webhook_gen.Make (Conn)
  open Replay.Fold

  let run ~(pipeline : string) ~(branch : string) query v f =
    with_db "Debug_db.run" (fun dbd ->
        let acc = "" in
        match%lwt query dbd ~_0:v ~pipeline ~branch f acc with
        | "" -> Lwt.return "No results, please check your query"
        | x -> Lwt.return x)

  let get_by_sha = run get_by_sha
  let get_by_build_number n = run get_by_build_number (Int64.of_string n)
  let get_after n = run get_after (Int64.of_string n)
  let get_from_to ~from ~to_ _ =
    (* we need to reverse the order of the labeled arguments because of the way we use the run function
       with the ~_0 arg from sqlgg. The from arg matches ~_0 and to_ matches ~_1 *)
    run (get_from_to ~to_:(Int64.of_string to_)) (Int64.of_string from)
end

let init ?(max_conn = 10) db_path =
  let%lwt p = Conn.Pool.create ~max_conn db_path in
  pool := Some p;
  available := Available;
  match%lwt with_db "Database.init" Failed_builds.T.ensure_failed_builds_webhook_table with
  | Ok _ -> Lwt.return_unit
  | Error e -> failwith e
  | Db_unavailable ->
    (* this should't happen *)
    assert false
