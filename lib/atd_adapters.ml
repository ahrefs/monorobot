module List_or_default_field = struct
  open Atdgen_runtime.Json_adapter

  module type Param = sig
    (** the name of the field *)
    val field_name : string

    (** string alias a user can use to denote default value *)
    val default_alias : string

    (** Yojson representation of default value *)
    val default_value : Yojson.Safe.t
  end

  module Make (P : Param) : S = struct
    open P

    let assoc_replace l k v = List.map (fun x -> if fst x = k then k, v else x) l

    let normalize x =
      match x with
      | `Assoc fields ->
        begin match List.assoc field_name fields with
        | `String s when String.equal s default_alias -> `Assoc (assoc_replace fields field_name default_value)
        | _ | (exception Not_found) -> x
        end
      | malformed -> malformed

    let restore x =
      match x with
      | `Assoc fields ->
        begin match List.assoc field_name fields with
        | value when Yojson.Safe.equal value default_value -> `Assoc (assoc_replace fields field_name (`String "any"))
        | _ | (exception Not_found) -> x
        end
      | malformed -> malformed
  end
end

module Branch_filters_adapter = List_or_default_field.Make (struct
  let field_name = "branch_filters"
  let default_alias = "any"
  let default_value = `List []
end)

(* Prefix and label rules declare where to route notifications with either a
   [channel] or a [user] field. This adapter folds whichever is present into the
   [target] variant field, so that existing configs keep working unchanged. *)
module Target_adapter : Atdgen_runtime.Json_adapter.S = struct
  let keys = [ "channel"; "user" ]

  let normalize (x : Yojson.Safe.t) =
    match x with
    | `Assoc fields ->
      begin match List.partition (fun (k, _) -> List.mem k keys) fields with
      | [], _ -> x
      | target, rest -> `Assoc (("target", `Assoc target) :: rest)
      end
    | _ -> x

  let restore (x : Yojson.Safe.t) =
    match x with
    | `Assoc fields ->
      begin match List.assoc "target" fields with
      | `Assoc [ target ] -> `Assoc (target :: List.remove_assoc "target" fields)
      | _ | (exception Not_found) -> x
      end
    | _ -> x
end

module Prefix_rule_adapter : Atdgen_runtime.Json_adapter.S = struct
  let normalize x = Target_adapter.normalize (Branch_filters_adapter.normalize x)
  let restore x = Branch_filters_adapter.restore (Target_adapter.restore x)
end

(** Error detection in Slack API response. The web API communicates errors using
    an [error] field rather than status codes. Note, on the other hand, that
    webhooks do use status codes to communicate errors. *)
module Slack_response_adapter : Atdgen_runtime.Json_adapter.S = struct
  let normalize (x : Yojson.Safe.t) =
    match x with
    | `Assoc fields ->
      begin match List.assoc "ok" fields with
      | `Bool true -> `List [ `String "Ok"; x ]
      | `Bool false ->
        begin match List.assoc "error" fields with
        | `String msg -> `List [ `String "Error"; `String msg ]
        | _ -> x
        end
      | _ | (exception Not_found) -> x
      end
    | _ -> x

  let restore (x : Yojson.Safe.t) =
    let mk_fields ok fields = ("ok", `Bool ok) :: List.filter (fun (k, _) -> k <> "ok") fields in
    match x with
    | `List [ `String "Ok"; `Assoc fields ] -> `Assoc (mk_fields true fields)
    | `List [ `String "Error"; `String msg ] -> `Assoc (mk_fields false [ "error", `String msg ])
    | _ -> x
end

(* This adapter is meant to avoid breaking changes in the config because the type for
   [allowed_pipelines] was changed from a string list to a pipeline record list. *)
module Strings_to_pipelines_adapter : Atdgen_runtime.Json_adapter.S = struct
  let normalize (x : Yojson.Safe.t) =
    match x with
    | `String s -> `Assoc [ "name", `String s ]
    | _ -> x

  let restore (x : Yojson.Safe.t) =
    match x with
    | `Assoc [ ("name", `String s) ] -> `String s
    | _ -> x
end

(* This adapter is meant to avoid breaking changes in the config because the type for
   [repo_config] was changed and [gh_token] was removed and [auth] was added. *)
module GH_token_to_auth_adapter : Atdgen_runtime.Json_adapter.S = struct
  let normalize (x : Yojson.Safe.t) =
    match x with
    | `Assoc ks ->
      `Assoc
        (List.map
           (fun (k, v) ->
             match k with
             | "gh_token" -> "auth", `List [ `String "GH_token"; v ]
             | _ -> k, v)
           ks)
    | _ -> x

  let restore (x : Yojson.Safe.t) = x
end
