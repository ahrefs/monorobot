open Base
open Rule_t

module Status = struct
  let default_rules =
    [
      { trigger = [ Pending ]; condition = None; policy = Ignore };
      { trigger = [ Failure; Error ]; condition = None; policy = Allow };
      { trigger = [ Success ]; condition = None; policy = Allow_once };
    ]

  (** `match_rules n rs` returns the policy declared by the first rule in `rs`
      to match status notification `n` if one exists, falling back to
      the default rules otherwise. A rule `r` matches `n` if `n.state` is in
      `r.trigger` and `n` meets `r.condition`. *)
  let match_rules (notification : Github_t.status_notification) ~rules =
    let match_rule rule =
      let value_of_field = function
        | Context -> Some notification.context
        | Description -> notification.description
        | Target_url -> notification.target_url
      in
      let rec match_condition = function
        | Match { field; re } ->
          value_of_field field
          |> Option.map ~f:(fun f -> Re.Str.string_match (Re.Str.regexp_case_fold re) f 0)
          |> Option.value ~default:false
        | All_of conditions -> List.for_all conditions ~f:match_condition
        | One_of conditions -> List.exists conditions ~f:match_condition
        | Not condition -> not @@ match_condition condition
      in
      if
        List.exists ~f:(Poly.equal notification.state) rule.trigger
        && rule.condition |> Option.map ~f:match_condition |> Option.value ~default:true
      then Some rule.policy
      else None
    in
    List.find_map (List.append rules default_rules) ~f:match_rule
end

module Prefix = struct
  (** `match_rules f rs` returns the channel name of a rule in `rs` that matches
      file name `f` with the longest prefix, if one exists. A rule `r` matches
      `f` with prefix length `l`, if `f` has no prefix in `r.ignore` and `l` is
      the length of the longest prefix of `f` in `r.allow`. An undefined or empty
      allow list is considered a prefix match of length 0. The ignore list is
      evaluated before the allow list. *)
  let match_rules filename ~rules =
    let compare a b = Int.compare (snd a) (snd b) in
    let is_prefix prefix = String.is_prefix filename ~prefix in
    let match_rule (rule : prefix_rule) =
      match rule.ignore with
      | Some ignore_list when List.exists ignore_list ~f:is_prefix -> None
      | _ ->
      match rule.allow with
      | None | Some [] -> Some (rule, 0)
      | Some allow_list ->
        allow_list
        |> List.filter_map ~f:(fun p -> if is_prefix p then Some (rule, String.length p) else None)
        |> List.max_elt ~compare
    in
    rules
    |> List.filter_map ~f:match_rule
    |> List.max_elt ~compare
    |> Option.map ~f:(fun (res : prefix_rule * int) -> (fst res).channel_name)

  let print_prefix_routing rules =
    let log = Devkit.Log.from "context" in
    let show_match l = String.concat ~sep:" or " @@ List.map ~f:(fun s -> s ^ "*") l in
    rules
    |> List.iter ~f:(fun (rule : prefix_rule) ->
         begin
           match rule.allow, rule.ignore with
           | None, None -> log#info "  any"
           | None, Some [] -> log#info "  any"
           | None, Some l -> log#info "  not %s" (show_match l)
           | Some l, None -> log#info "  %s" (show_match l)
           | Some l, Some [] -> log#info "  %s" (show_match l)
           | Some l, Some i -> log#info "  %s and not %s" (show_match l) (show_match i)
         end;
         log#info " -> #%s\n%!" rule.channel_name)
end

module Label = struct
  (** `match_rules l rs` returns the channel names of the rules in `rs` that
      allow label `l`, if one exists. A rule `r` matches label `l`, if `l` is
      not a member of `r.ignore` and is a member of `r.allow`. The label name
      comparison is case insensitive. An undefined allow list is considered a
      match. The ignore list is evaluated before the allow list. *)
  let match_rules (label : Github_t.label) ~rules =
    let label_name = String.lowercase label.name in
    let label_name_equal name = String.equal label_name (String.lowercase name) in
    let match_rule rule =
      match rule.ignore with
      | Some ignore_list when List.exists ignore_list ~f:label_name_equal -> None
      | _ ->
      match rule.allow with
      | None | Some [] -> Some rule.channel_name
      | Some allow_list -> if List.exists allow_list ~f:label_name_equal then Some rule.channel_name else None
    in
    rules |> List.filter_map ~f:match_rule |> List.dedup_and_sort ~compare:String.compare

  let print_label_routing rules =
    let log = Devkit.Log.from "context" in
    let show_match l = String.concat ~sep:" or " l in
    rules
    |> List.iter ~f:(fun (rule : label_rule) ->
         begin
           match rule.allow, rule.ignore with
           | None, None -> log#info "  any"
           | None, Some [] -> log#info "  any"
           | None, Some l -> log#info "  not %s" (show_match l)
           | Some l, None -> log#info "  %s" (show_match l)
           | Some l, Some [] -> log#info "  %s" (show_match l)
           | Some l, Some i -> log#info "  %s and not %s" (show_match l) (show_match i)
         end;
         log#info " -> #%s\n%!" rule.channel_name)
end
