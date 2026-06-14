open Printf
open Monorobotlib

let job_id = "019e6dcb-4ec9-4c24-ad14-8a8099e30556"

let lines first_line last_line = { Util.Build.first_line; last_line }
let fragment ?lines job_id = { Util.Build.job_id; lines }

let buildkite_link ?(original_url = "") ?(org = "ahrefs") ?(pipeline = "monorepo") ?fragment build_nr =
  let original_url =
    if original_url = "" then Util.Build.buildkite_build_url ~org ~pipeline ~build_nr () else original_url
  in
  {
    Util.Build.original_url;
    org;
    pipeline;
    build_nr;
    build_url = Util.Build.buildkite_build_url ~org ~pipeline ~build_nr ();
    fragment;
  }

let cases =
  let build_url = "https://buildkite.com/ahrefs/monorepo/builds/327273" in
  [
    build_url, Some (buildkite_link "327273");
    ( build_url ^ "?foo=bar",
      Some (buildkite_link ~original_url:(build_url ^ "?foo=bar") "327273") );
    ( build_url ^ "#" ^ job_id,
      Some (buildkite_link ~original_url:(build_url ^ "#" ^ job_id) ~fragment:(fragment job_id) "327273") );
    ( build_url ^ "#" ^ job_id ^ "/L2448",
      Some
        (buildkite_link
           ~original_url:(build_url ^ "#" ^ job_id ^ "/L2448")
           ~fragment:(fragment ~lines:(lines 2448 2448) job_id)
           "327273") );
    ( build_url ^ "#" ^ job_id ^ "/L2197-L2203",
      Some
        (buildkite_link
           ~original_url:(build_url ^ "#" ^ job_id ^ "/L2197-L2203")
           ~fragment:(fragment ~lines:(lines 2197 2203) job_id)
           "327273") );
    "https://example.com/ahrefs/monorepo/builds/327273", None;
    "https://buildkite.com/ahrefs/monorepo", None;
    "https://buildkite.com/ahrefs/monorepo/builds/nope", None;
    build_url ^ "#" ^ job_id ^ "/L0", None;
    build_url ^ "#" ^ job_id ^ "/L4-L2", None;
  ]

let output = function
  | None -> "{None}"
  | Some { Util.Build.original_url; org; pipeline; build_nr; build_url; fragment } ->
    let fragment =
      match fragment with
      | None -> "none"
      | Some { job_id; lines = None } -> sprintf "%s" job_id
      | Some { job_id; lines = Some { first_line; last_line } } ->
        sprintf "%s/L%d-L%d" job_id first_line last_line
    in
    sprintf "%s %s/%s/%s %s %s" original_url org pipeline build_nr build_url fragment

let () =
  List.iter
    (fun (input, expected) ->
      let got = Util.Build.buildkite_link_of_string input in
      assert (
        match got = expected with
        | true -> true
        | false ->
          Printf.printf "for: %s | expected: %s but got %s" input (output expected) (output got);
          false))
    cases
