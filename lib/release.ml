open Devkit

type t = {
  commits : string list;
  note : string option;
  previous_release : string option;
}

let previous_release_prefix = "Previous-Release:"
let pull_request_re = Re2.create_exn {|\(#([0-9]+)\)|}

(* git appends the signature of a signed tag to the tag object's message, and the API hands it over with the rest *)
let is_signature_start line =
  String.starts_with line ~prefix:"-----BEGIN " && String.ends_with line ~suffix:"SIGNATURE-----"

let parse message =
  let lines = String.split_on_char '\n' message |> List.map String.trim in
  let rec take_until_signature acc = function
    | [] -> List.rev acc
    | line :: _ when is_signature_start line -> List.rev acc
    | line :: rest -> take_until_signature (line :: acc) rest
  in
  let lines = take_until_signature [] lines in
  let previous_release =
    List.find_map
      (fun line ->
        match String.starts_with line ~prefix:previous_release_prefix with
        | false -> None
        | true -> Some (Stre.drop_prefix line previous_release_prefix |> String.trim))
      lines
  in
  (* Commits run until the first blank line. Anything after it is the -m suffix
     release.sh appends, or the trailer. *)
  let rec take_commits acc = function
    | [] -> List.rev acc
    | "" :: _ -> List.rev acc
    | line :: _ when String.starts_with line ~prefix:previous_release_prefix -> List.rev acc
    | line :: rest -> take_commits (line :: acc) rest
  in
  (* What the releaser passed to release.sh with -m, if anything. It sits
     between the commit list and the trailer, and says what the release is for
     in a way the commit subjects do not. *)
  let rec after_first_blank = function
    | [] -> []
    | "" :: rest -> rest
    | _ :: rest -> after_first_blank rest
  in
  let note =
    after_first_blank lines
    |> List.filter (fun line ->
      (not (String.equal line "")) && not (String.starts_with line ~prefix:previous_release_prefix))
    |> function
    | [] -> None
    | lines -> Some (String.concat "\n" lines)
  in
  { commits = take_commits [] lines; note; previous_release }

let commit_count t = List.length t.commits

let link_pull_requests ~repo_url line =
  Re2.replace_exn pull_request_re line ~f:(fun m ->
    let number = Re2.Match.get_exn ~sub:(`Index 1) m in
    Printf.sprintf "(<%s/pull/%s|#%s>)" repo_url number number)
