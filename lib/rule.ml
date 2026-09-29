open Common
open Rule_t

module Status = struct
  let default_rules =
    [
      { trigger = [ Pending ]; condition = None; policy = Ignore; notify_channels = false; notify_dm = false };
      { trigger = [ Failure; Error ]; condition = None; policy = Allow; notify_channels = true; notify_dm = false };
      { trigger = [ Success ]; condition = None; policy = Allow_once; notify_channels = true; notify_dm = false };
    ]

  (** [match_rules n rs] returns the policy declared by the first rule in [rs]
      to match status notification [n] if one exists, falling back to
      the default rules otherwise. A rule [r] matches [n] if [n.state] is in
      [r.trigger] and [n] meets [r.condition]. *)
  let match_rules (notification : Github_t.status_notification) ~rules =
    let match_rule rule =
      let value_of_field = function
        | Context -> Some notification.context
        | Description -> notification.description
        | Target_url -> notification.target_url
      in
      let rec match_condition = function
        | Match { field; re } -> value_of_field field |> Option.map (Re2.matches re) |> Option.default false
        | All_of conditions -> List.for_all match_condition conditions
        | One_of conditions -> List.exists match_condition conditions
        | Not condition -> not @@ match_condition condition
      in
      if
        List.exists (( = ) notification.state) rule.trigger
        && rule.condition |> Option.map match_condition |> Option.default true
      then Some (rule.policy, rule.notify_channels, rule.notify_dm)
      else None
    in
    List.find_map match_rule (List.append rules default_rules)
end

(** Where a matching rule routes notifications: a channel, or a direct message to
    the user with the given Slack email. *)
module Target = struct
  type t =
    | Channel of Slack_channel.Name.t
    | User of string

  let of_rule ~channels ~dms = List.map (fun c -> Channel c) channels @ List.map (fun u -> User u) dms

  let compare a b =
    match a, b with
    | Channel a, Channel b -> Slack_channel.compare a b
    | User a, User b -> String.compare a b
    | Channel _, User _ -> -1
    | User _, Channel _ -> 1

  let to_string = function
    | Channel c -> "#" ^ Slack_channel.Name.project c
    | User u -> "@" ^ u

  let print_all targets = String.concat ", " (List.map to_string targets)
end

module Prefix = struct
  (** Filters prefix rules based on branch filtering config and current commit.
      Prioritizes local filters over main branch one. Only allows distinct commits
      if no filter is matched. *)
  let filter_by_branch ~branch ~main_branch ~distinct rule =
    match rule.branch_filters with
    | Some (_ :: _ as filters) -> List.mem branch filters
    | Some [] -> distinct
    | None ->
    match main_branch with
    | Some main_branch -> String.equal main_branch branch
    | None -> distinct

  (** [match_rules f rs] returns the targets of the rule in [rs] that matches
      file name [f] with the longest prefix, or [[]] if none does. A rule [r] matches
      [f] with prefix length [l], if [f] has no prefix in [r.ignore] and [l] is
      the length of the longest prefix of [f] in [r.allow]. An undefined or empty
      allow list is considered a prefix match of length 0. The ignore list is
      evaluated before the allow list. *)
  let match_rules filename ~rules =
    let max_elt t =
      let compare a b = Int.compare (snd a) (snd b) in
      match t with
      | [] -> None
      | v :: vs -> Some (List.fold_left (fun a b -> if compare b a > 0 then b else a) v vs)
    in
    let is_prefix prefix = String.starts_with filename ~prefix in
    let match_rule (rule : prefix_rule) =
      match rule.ignore with
      | Some ignore_list when List.exists is_prefix ignore_list -> None
      | _ ->
      match rule.allow with
      | None | Some [] -> Some (rule, 0)
      | Some allow_list ->
        allow_list |> List.filter_map (fun p -> if is_prefix p then Some (rule, String.length p) else None) |> max_elt
    in
    match rules |> List.filter_map match_rule |> max_elt with
    | None -> []
    | Some ({ channels; dms; _ }, _) -> Target.of_rule ~channels ~dms

  let print_prefix_routing rules =
    let show_match l = String.concat " or " @@ List.map (fun s -> s ^ "*") l in
    rules
    |> List.iter (fun (rule : prefix_rule) ->
      begin match rule.allow, rule.ignore with
      | None, None -> Printf.printf "  any"
      | None, Some [] -> Printf.printf "  any"
      | None, Some l -> Printf.printf "  not %s" (show_match l)
      | Some l, None -> Printf.printf "  %s" (show_match l)
      | Some l, Some [] -> Printf.printf "  %s" (show_match l)
      | Some l, Some i -> Printf.printf "  %s and not %s" (show_match l) (show_match i)
      end;
      Printf.printf " -> %s\n%!" (Target.print_all (Target.of_rule ~channels:rule.channels ~dms:rule.dms)))
end

module Label = struct
  (** [match_rules l rs] returns the targets of the rules in [rs] that
      allow label [l], if one exists. A rule [r] matches label [l], if [l] is
      not a member of [r.ignore] and is a member of [r.allow]. The label name
      comparison is case insensitive. An undefined allow list is considered a
      match. The ignore list is evaluated before the allow list. *)
  let match_rules (label : Github_t.label) ~rules =
    let label_name = String.lowercase_ascii label.name in
    let label_name_equal name = String.equal label_name (String.lowercase_ascii name) in
    let match_rule rule =
      match rule.ignore with
      | Some ignore_list when List.exists label_name_equal ignore_list -> false
      | _ ->
      match rule.allow with
      | None | Some [] -> true
      | Some allow_list -> List.exists label_name_equal allow_list
    in
    rules
    |> List.filter match_rule
    |> List.concat_map (fun { channels; dms; _ } -> Target.of_rule ~channels ~dms)
    |> List.sort_uniq Target.compare

  let print_label_routing rules =
    let show_match l = String.concat " or " l in
    rules
    |> List.iter (fun (rule : label_rule) ->
      begin match rule.allow, rule.ignore with
      | None, None -> Printf.printf "  any"
      | None, Some [] -> Printf.printf "  any"
      | None, Some l -> Printf.printf "  not %s" (show_match l)
      | Some l, None -> Printf.printf "  %s" (show_match l)
      | Some l, Some [] -> Printf.printf "  %s" (show_match l)
      | Some l, Some i -> Printf.printf "  %s and not %s" (show_match l) (show_match i)
      end;
      Printf.printf " -> %s\n%!" (Target.print_all (Target.of_rule ~channels:rule.channels ~dms:rule.dms)))
end

module Project_owners = struct
  let match_rules pr_labels rules =
    match pr_labels with
    | [] -> []
    | _ :: _ ->
      let pr_labels_set = List.map (fun (l : Github_t.label) -> l.name) pr_labels |> StringSet.of_list in
      List.fold_left
        (fun results_set { label; labels; owners } ->
          match owners with
          | [] -> results_set
          | _ :: _ ->
          match Stdlib.Option.to_list label @ labels with
          | [] -> results_set
          | labels ->
          match StringSet.subset (StringSet.of_list labels) pr_labels_set with
          | false -> results_set
          | true -> List.fold_left (fun a s -> StringSet.add s a) results_set owners)
        StringSet.empty rules
      |> StringSet.to_seq
      |> List.of_seq
end
