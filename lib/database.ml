type db_status =
  | Not_available
  | Uninitialized
  | Initialized

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
      mutable connections : connection list;
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

let with_db (f : connection -> 'a Lwt.t) : 'a db_use_result Lwt.t =
  match !available with
  | Uninitialized ->
    (* we should never be in this state when this function is called *)
    assert false
  | Not_available -> Lwt.return Db_unavailable
  | Initialized ->
    let pool = Option.get !pool in
    (match%lwt Conn.Pool.use pool f with
    | x -> Lwt.return (Ok x)
    | exception e -> Lwt.return (Error (Printexc.to_string e)))

module Status_notifications_table = struct
  include Status_notifications_gen.Make (Conn)

  type n = Github_t.status_notification

  let id' (n : n) = Int64.of_int n.id

  (* We need to have ~is_step_notification, ~is_canceled, and ~last_handled_in as arguments
     to avoid circular dependencies with the Util module *)
  let init (n : n) ~notification_text ~build_number ~is_step_notification ~is_canceled last_handled_in =
    let { Github_t.commit; state; description; target_url; context; branches; updated_at; _ } = n in
    let { Github_t.commit : Github_t.inner_commit; sha; html_url; _ } = commit in
    let ({ Github_t.author; _ } : Github_t.inner_commit) = commit in
    try%lwt
      match target_url, description with
      | None, _ | _, None ->
        (* We don't support notifications without a build url or description *)
        failwith "missing build url and/or description in the status notification"
      | Some build_url, Some description ->
        let branch =
          match branches with
          | [] -> failwith "no branches found in the status notification"
          | [ branch ] -> branch.name
          | _ -> failwith "multiple branches are not supported in status notification"
        in
        let repository = n.repository.url in
        let state_before_notification = "{}" in
        let state_after_notification = "{}" in
        let updated_at = Common.Timestamp.wrap_with_fallback updated_at |> Ptime.to_float_s in
        let meta_created_at = updated_at in
        let meta_updated_at = updated_at in
        let notification_text =
          (* remove the commit and repository fields from the notification json text without having to manually
             create a new record and serialize it *)
          Debug_db_j.(status_notification_of_string notification_text |> string_of_status_notification)
        in
        with_db
          (insert ~id:(id' n) ~notification_text ~sha ~commit_author:author.email ~commit_url:html_url
             ~n_state:(Github_j.string_of_status_state state) ~description ~target_url:(Option.get target_url)
             ~build_url ~build_number ~is_step_notification ~is_canceled ~context ~repository ~branch ~last_handled_in
             ~updated_at ~state_before_notification ~state_after_notification ~meta_created_at ~meta_updated_at
             ~matched_rule:"" ~has_state_update:false)
    with e ->
      let exn_str = Printexc.to_string e in
      log#error "failed to create status notification: %s" exn_str;
      Lwt.return (Error exn_str)

  let last_handled_in (n : n) last_handled_in = with_db (update_last_handled_in ~id:(id' n) ~last_handled_in)

  let update_matched_rule (n : n) rule = with_db (update_matched_rule ~id:(id' n) ~rule)

  let update_state (n : n) ?before ~after last_handled_in =
    let before = Option.map_default State_j.string_of_pipeline_statuses "{}" before in
    let after = State_j.string_of_pipeline_statuses after in
    with_db (update_state ~id:(id' n) ~before ~after ~last_handled_in ~has_state_update:(before <> after))
end

module Debug_db = struct
  module Replay = Status_notifications_gen.Make (Conn)
  open Replay.Fold

  let run ~pipeline ~branch query v f = with_db (fun dbd -> query dbd ~_0:v ~pipeline ~branch f "")

  let get_by_sha = run get_by_sha
  let get_by_build_number n = run get_by_build_number (Int64.of_string n)
  let get_by_range n = run get_by_range (Int64.of_string n)
  let get_by_step_name = run get_by_step_name
end

let init ?(max_conn = 10) db_path =
  let%lwt p = Conn.Pool.create ~max_conn db_path in
  pool := Some p;
  available := Initialized;
  match%lwt with_db Status_notifications_table.ensure_status_notifications_table with
  | Ok _ -> Lwt.return_unit
  | Error e -> failwith e
  | Db_unavailable ->
    (* this should't happen *)
    assert false
