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
    match List.exists ctx.cfg.status_rules.status ~f:(Poly.equal Config.HideConsecutiveSuccess) with
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
  type prefix_match =
    | Match of int
    | NoMatch

  let chan_of_prefix_rule (r : prefix_rule) = r.channel_name

  let touching_prefix (rule : prefix_rule) name =
    let match_lengths filename prefixes =
      List.filter_map
        ~f:(fun prefix -> if String.is_prefix filename ~prefix then Some (String.length prefix) else None)
        prefixes
    in
    match match_lengths name rule.ignore with
    | _ :: _ -> NoMatch
    | [] ->
    match rule.allow with
    | [] -> Match 0
    | _ ->
    match List.max_elt (match_lengths name rule.allow) ~compare:Int.compare with
    | Some x -> Match x
    | None -> NoMatch

  let longest_touching_prefix_rule rules name =
    let get_m rule = touching_prefix rule name in
    let reduce_to_longest_match longest_rule_match_pair current_rule =
      let _, longest_match = longest_rule_match_pair in
      let current_match = get_m current_rule in
      let current_rule_match_pair = current_rule, current_match in
      match longest_match with
      | NoMatch -> current_rule_match_pair
      | Match x ->
      match current_match with
      | NoMatch -> longest_rule_match_pair
      | Match y -> if y > x then current_rule_match_pair else longest_rule_match_pair
    in
    match rules with
    | [] -> None
    | (x : prefix_rule) :: xs ->
    match List.fold_left xs ~init:(x, get_m x) ~f:reduce_to_longest_match with
    | _, NoMatch -> None
    | r, Match _ -> Some r

  let chan_of_file rules file = Option.map ~f:chan_of_prefix_rule @@ longest_touching_prefix_rule rules file

  let unique_chans_of_files rules files =
    List.dedup_and_sort ~compare:String.compare @@ List.filter_map files ~f:(chan_of_file rules)

  let filter_push rules (commit : Github_t.commit) =
    let files = List.concat [ commit.added; commit.removed; commit.modified ] in
    List.map ~f:(fun chan -> chan, commit) @@ unique_chans_of_files rules files

  let print_prefix_routing rules =
    let show_match l = String.concat ~sep:" or " @@ List.map ~f:(fun s -> s ^ "*") l in
    rules
    |> List.iter ~f:(fun (rule : prefix_rule) ->
         begin
           match rule.allow, rule.ignore with
           | [], [] -> Stdio.printf "  any"
           | l, [] -> Stdio.printf "  %s" (show_match l)
           | [], l -> Stdio.printf "  not %s" (show_match l)
           | l, i -> Stdio.printf "  %s and not %s" (show_match l) (show_match i)
         end;
         Stdio.printf " -> #%s\n%!" rule.channel_name)
end

module Label = struct
  let touching_label rule name =
    let name_lc = String.lowercase name in
    let label_lc = List.map rule.allow ~f:(fun l -> String.lowercase l) in
    let ignore_lc = List.map rule.ignore ~f:(fun l -> String.lowercase l) in
    (* convert both labels and config into lowe-case to make label matching case-insensitive *)
    (List.is_empty label_lc || List.mem ~equal:String.equal label_lc name_lc)
    && not (List.mem ~equal:String.equal ignore_lc name_lc)

  let filter_label rules (label : Github_j.label) =
    rules
    |> List.filter_map ~f:(fun rule ->
         match touching_label rule label.name with
         | false -> None
         | true -> Some rule.channel_name)

  let print_label_routing rules =
    let show_match l = String.concat ~sep:" or " l in
    rules
    |> List.iter ~f:(fun (rule : label_rule) ->
         begin
           match rule.allow, rule.ignore with
           | [], [] -> Stdio.printf "  any"
           | l, [] -> Stdio.printf "  %s" (show_match l)
           | [], l -> Stdio.printf "  not %s" (show_match l)
           | l, i -> Stdio.printf "  %s and not %s" (show_match l) (show_match i)
         end;
         Stdio.printf " -> #%s\n%!" rule.channel_name)
end
