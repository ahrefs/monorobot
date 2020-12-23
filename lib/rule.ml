open Base
open Rule_t
open Github_t

module Status = struct
  let hide_cancelled (notification : status_notification) (cfg : Config.t) =
    let find_cancelled status_state =
      match status_state with
      | Config.Cancelled r -> Some r
      | _ -> None
    in
    let regexp_opt = List.find_map cfg.status_rules.status ~f:find_cancelled in
    match regexp_opt with
    | None -> false
    | Some regexp ->
      let { state; description; _ } = notification in
      let r = Re.Str.regexp_case_fold regexp in
      ( match description, state with
      | Some s, Failure when Re.Str.string_match r s 0 -> true
      | _ -> false
      )

  let hide_success (n : status_notification) (ctx : Context.t) =
    match ctx.config with
    | None -> raise @@ Failure "no config file defined"
    | Some cfg ->
    match List.exists cfg.status_rules.status ~f:(Poly.equal Config.HideConsecutiveSuccess) with
    | false -> false
    | true ->
    match n.state with
    | Success ->
      List.exists
        ~f:(fun b ->
          match State.get_branch_state b.name ctx.state with
          | None | Some { last_build_state = Failure; _ } -> false
          | Some { last_build_state = Success; _ } -> true)
        n.branches
    | _ -> false
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
    let show_match l = String.concat ~sep:" or " @@ List.map ~f:(fun s -> s ^ "*") l in
    rules
    |> List.iter ~f:(fun (rule : prefix_rule) ->
         begin
           match rule.allow, rule.ignore with
           | None, None -> Stdio.printf "  any"
           | None, Some [] -> Stdio.printf "  any"
           | None, Some l -> Stdio.printf "  not %s" (show_match l)
           | Some l, None -> Stdio.printf "  %s" (show_match l)
           | Some l, Some [] -> Stdio.printf "  %s" (show_match l)
           | Some l, Some i -> Stdio.printf "  %s and not %s" (show_match l) (show_match i)
         end;
         Stdio.printf " -> #%s\n%!" rule.channel_name)
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
    let show_match l = String.concat ~sep:" or " l in
    rules
    |> List.iter ~f:(fun (rule : label_rule) ->
         begin
           match rule.allow, rule.ignore with
           | None, None -> Stdio.printf "  any"
           | None, Some [] -> Stdio.printf "  any"
           | None, Some l -> Stdio.printf "  not %s" (show_match l)
           | Some l, None -> Stdio.printf "  %s" (show_match l)
           | Some l, Some [] -> Stdio.printf "  %s" (show_match l)
           | Some l, Some i -> Stdio.printf "  %s and not %s" (show_match l) (show_match i)
         end;
         Stdio.printf " -> #%s\n%!" rule.channel_name)
end
