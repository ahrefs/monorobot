open Devkit
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
        List.exists ((==) notification.state) rule.trigger
        && rule.condition |> Option.map match_condition |> Option.default true
      then Some (rule.policy, rule.notify_channels, rule.notify_dm)
      else None
    in
    List.find_map match_rule (List.append rules default_rules)
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

  (** [match_rules f rs] returns the channel name of a rule in [rs] that matches
      file name [f] with the longest prefix, if one exists. A rule [r] matches
      [f] with prefix length [l], if [f] has no prefix in [r.ignore] and [l] is
      the length of the longest prefix of [f] in [r.allow]. An undefined or empty
      allow list is considered a prefix match of length 0. The ignore list is
      evaluated before the allow list. *)
  let match_rules filename ~rules =
    let max_elt t =
      let compare a b = Int.compare (snd a) (snd b) in
      List.fold_left (fun a b -> if a = None || compare b (Option.get a) > 0 then Some b else a) None t in
    let is_prefix prefix = Stre.starts_with filename prefix in
    let match_rule (rule : prefix_rule) =
      match rule.ignore with
      | Some ignore_list when List.exists is_prefix ignore_list -> None
      | _ ->
      match rule.allow with
      | None | Some [] -> Some (rule, 0)
      | Some allow_list ->
        allow_list
        |> List.filter_map (fun p -> if is_prefix p then Some (rule, String.length p) else None)
        |> max_elt
    in
    rules
    |> List.filter_map match_rule
    |> max_elt
    |> Option.map (fun (res : prefix_rule * int) -> (fst res).channel_name)

  let print_prefix_routing rules =
    let show_match l = String.concat " or " @@ List.map (fun s -> s ^ "*") l in
    rules
    |> List.iter (fun (rule : prefix_rule) ->
         begin
           match rule.allow, rule.ignore with
           | None, None -> Stdio.printf "  any"
           | None, Some [] -> Stdio.printf "  any"
           | None, Some l -> Stdio.printf "  not %s" (show_match l)
           | Some l, None -> Stdio.printf "  %s" (show_match l)
           | Some l, Some [] -> Stdio.printf "  %s" (show_match l)
           | Some l, Some i -> Stdio.printf "  %s and not %s" (show_match l) (show_match i)
         end;
         Stdio.printf " -> #%s\n%!" rule.channel_name
       )
end

module Label = struct
  (** [match_rules l rs] returns the channel names of the rules in [rs] that
      allow label [l], if one exists. A rule [r] matches label [l], if [l] is
      not a member of [r.ignore] and is a member of [r.allow]. The label name
      comparison is case insensitive. An undefined allow list is considered a
      match. The ignore list is evaluated before the allow list. *)
  let match_rules (label : Github_t.label) ~rules =
    let label_name = String.lowercase_ascii label.name in
    let label_name_equal name = String.equal label_name (String.lowercase_ascii name) in
    let match_rule rule =
      match rule.ignore with
      | Some ignore_list when List.exists label_name_equal ignore_list -> None
      | _ ->
      match rule.allow with
      | None | Some [] -> Some rule.channel_name
      | Some allow_list -> if List.exists label_name_equal allow_list then Some rule.channel_name else None
    in
    rules |> List.filter_map match_rule |> dedup_and_sort ~compare:String.compare

  let print_label_routing rules =
    let show_match l = String.concat " or " l in
    rules
    |> List.iter (fun (rule : label_rule) ->
         begin
           match rule.allow, rule.ignore with
           | None, None -> Stdio.printf "  any"
           | None, Some [] -> Stdio.printf "  any"
           | None, Some l -> Stdio.printf "  not %s" (show_match l)
           | Some l, None -> Stdio.printf "  %s" (show_match l)
           | Some l, Some [] -> Stdio.printf "  %s" (show_match l)
           | Some l, Some i -> Stdio.printf "  %s and not %s" (show_match l) (show_match i)
         end;
         Stdio.printf " -> #%s\n%!" rule.channel_name
       )
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
          | true -> List.fold_left (fun a s -> StringSet.add s a) results_set owners
        )
        StringSet.empty
        rules
      |> StringSet.to_seq |> List.of_seq
end
