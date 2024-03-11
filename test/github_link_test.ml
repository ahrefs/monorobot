open Printf
open Lib
open Github
open Github_t

let mk_repo ?(owner = "ahrefs") ?(scheme = "https") prefix prefix_api : repository =
  {
    name = "monorepo";
    full_name = sprintf "%s/monorepo" owner;
    url = sprintf "%s://%s/%s/monorepo" scheme prefix owner;
    commits_url = sprintf "%s://%s/repos/%s/monorepo/commits{/sha}" scheme prefix_api owner;
    contents_url = sprintf "%s://%s/repos/%s/monorepo/contents/{+path}" scheme prefix_api owner;
    pulls_url = sprintf "%s://%s/repos/%s/monorepo/pulls{/number}" scheme prefix_api owner;
    issues_url = sprintf "%s://%s/repos/%s/monorepo/issues{/number}" scheme prefix_api owner;
    compare_url = sprintf "%s://%s/repos/%s/monorepo/compare{/basehead}" scheme prefix_api owner;
  }

let enterprise_repo1 = mk_repo "example.org" "example.org/api/v3"
let enterprise_repo2 = mk_repo "example.org/path/to/git" "example.org/path/to/git/api/v3"
let enterprise_repo_insecure = mk_repo ~scheme:"http" "example.org" "example.org/api/v3"
let github_repo = mk_repo "github.com" "api.github.com"

let pr_cases prefix repo =
  [
    sprintf "https://%s/ahrefs/monorepo/pull/100" prefix, Some (Pull_request (repo, 100));
    sprintf "https://%s/ahrefs/monorepo/pull/2" prefix, Some (Pull_request (repo, 2));
    sprintf "https://%s/ahrefs/monorepo/pull/100/" prefix, Some (Pull_request (repo, 100));
    sprintf "https://%s/ahrefs/monorepo/pull/100?arg1=123" prefix, Some (Pull_request (repo, 100));
    sprintf "https://%s/ahrefs/monorepo/pull/abc" prefix, None;
    sprintf "https://%s/ahrefs/monorepo/pull/" prefix, None;
    sprintf "https://%s/ahrefs/monorepo/pull" prefix, None;
  ]

let issue_cases prefix repo =
  [
    sprintf "https://%s/ahrefs/monorepo/issues/100" prefix, Some (Issue (repo, 100));
    sprintf "https://%s/ahrefs/monorepo/issues/2" prefix, Some (Issue (repo, 2));
    sprintf "https://%s/ahrefs/monorepo/issues/100/" prefix, Some (Issue (repo, 100));
    sprintf "https://%s/ahrefs/monorepo/issues/100?arg1=123" prefix, Some (Issue (repo, 100));
    sprintf "https://%s/ahrefs/monorepo/issues/abc" prefix, None;
    sprintf "https://%s/ahrefs/monorepo/issues/" prefix, None;
    sprintf "https://%s/ahrefs/monorepo/issues" prefix, None;
  ]

let commit_cases prefix repo =
  [
    ( sprintf "https://%s/ahrefs/monorepo/commit/0d09a6cb71481fe77cad7c7729d400ab40fd292e" prefix,
      Some (Commit (repo, "0d09a6cb71481fe77cad7c7729d400ab40fd292e")) );
    ( sprintf "https://%s/ahrefs/monorepo/commit/0d09a6cb71481fe77cad7c7729d400ab40fd292e/" prefix,
      Some (Commit (repo, "0d09a6cb71481fe77cad7c7729d400ab40fd292e")) );
    ( sprintf "https://%s/ahrefs/monorepo/commit/0d09a6cb71481fe77cad7c7729d400ab40fd292e?arg1=123" prefix,
      Some (Commit (repo, "0d09a6cb71481fe77cad7c7729d400ab40fd292e")) );
    ( sprintf "https://%s/ahrefs/monorepo/pull/2938/commits/0d09a6cb71481fe77cad7c7729d400ab40fd292e?arg1=123" prefix,
      Some (Commit (repo, "0d09a6cb71481fe77cad7c7729d400ab40fd292e")) );
    sprintf "https://%s/ahrefs/monorepo/commit/" prefix, None;
    sprintf "https://%s/ahrefs/monorepo/commit" prefix, None;
  ]

let compare_cases prefix repo =
  [
    sprintf "https://%s/ahrefs/monorepo/compare/master...develop" prefix, Some (Compare (repo, ("master", "develop")));
    sprintf "https://%s/ahrefs/monorepo/compare/develop...master/" prefix, Some (Compare (repo, ("develop", "master")));
    ( sprintf "https://%s/ahrefs/monorepo/compare/master...sewen/one-feature" prefix,
      Some (Compare (repo, ("master", "sewen/one-feature"))) );
    ( sprintf "https://%s/ahrefs/monorepo/compare/master...sewen/one-feature/" prefix,
      Some (Compare (repo, ("master", "sewen/one-feature"))) );
    ( sprintf "https://%s/ahrefs/monorepo/compare/sewen/one-feature...master/" prefix,
      Some (Compare (repo, ("sewen/one-feature", "master"))) );
    ( sprintf "https://%s/ahrefs/monorepo/compare/sewen/one-feature...other:master/" prefix,
      Some (Compare (repo, ("sewen/one-feature", "other:master"))) );
    ( sprintf "https://%s/ahrefs/monorepo/compare/master...other:monorobot:119-unfurling-commit-range-links/" prefix,
      Some (Compare (repo, ("master", "other:monorobot:119-unfurling-commit-range-links"))) );
    ( sprintf "https://%s/ahrefs/monorepo/compare/other:1-feature...master/" prefix,
      Some (Compare (repo, ("other:1-feature", "master"))) );
    ( sprintf "https://%s/ahrefs/monorepo/compare/other:0d09a6cb71481fe77cad7c7729d400ab40fd292e...master/" prefix,
      Some (Compare (repo, ("other:0d09a6cb71481fe77cad7c7729d400ab40fd292e", "master"))) );
    ( sprintf "https://%s/ahrefs/monorepo/compare/other:v1.0.0...master/" prefix,
      Some (Compare (repo, ("other:v1.0.0", "master"))) );
    ( sprintf "https://%s/ahrefs/monorepo/compare/0d09a6cb71481fe77cad7c7729d400ab40fd292e...master/" prefix,
      Some (Compare (repo, ("0d09a6cb71481fe77cad7c7729d400ab40fd292e", "master"))) );
    ( sprintf "https://%s/ahrefs/monorepo/compare/sewen/123_x-feature...master" prefix,
      Some (Compare (repo, ("sewen/123_x-feature", "master"))) );
    ( sprintf "https://%s/ahrefs/monorepo/compare/sewen/123ab^^^...master" prefix,
      Some (Compare (repo, ("sewen/123ab^^^", "master"))) );
    ( sprintf "https://%s/ahrefs/monorepo/compare/sewen/123ab~3...master" prefix,
      Some (Compare (repo, ("sewen/123ab~3", "master"))) );
    ( sprintf
        "https://%s/ahrefs/monorepo/pull/523/files/6260c936f9f6959c272aecb430a8a263915412c9...81f5a6c7af12d4b5af113d5372d1abd3743f65cb/"
        prefix,
      Some (Compare (repo, ("6260c936f9f6959c272aecb430a8a263915412c9", "81f5a6c7af12d4b5af113d5372d1abd3743f65cb"))) );
    ( sprintf "https://%s/ahrefs/monorepo/compare/" prefix ^ "abc%5E%5E%5E...ax~2",
      Some (Compare (repo, ("abc^^^", "ax~2"))) );
    sprintf "https://%s/ahrefs/monorepo/compare" prefix, None;
  ]

let other_cases =
  [ "http://github.com/ahrefs/monorepo/commit/", None; "http://example.org/ahrefs/monorepo/commit/", None; "abc", None ]

let cases =
  List.concat
    [
      pr_cases "github.com" github_repo;
      issue_cases "github.com" github_repo;
      commit_cases "github.com" github_repo;
      pr_cases "www.github.com" github_repo;
      issue_cases "www.github.com" github_repo;
      commit_cases "www.github.com" github_repo;
      compare_cases "github.com" github_repo;
      pr_cases "example.org" enterprise_repo1;
      issue_cases "example.org" enterprise_repo1;
      commit_cases "example.org" enterprise_repo1;
      compare_cases "example.org" enterprise_repo1;
      pr_cases "example.org/path/to/git" enterprise_repo2;
      issue_cases "example.org/path/to/git" enterprise_repo2;
      commit_cases "example.org/path/to/git" enterprise_repo2;
      compare_cases "example.org/path/to/git" enterprise_repo2;
      other_cases;
    ]

let gh_link_output = function
  | Some (Issue (repo, matched_re)) | Some (Pull_request (repo, matched_re)) ->
    sprintf "matched: %d \nfor repo %s\n" matched_re
      (repo |> Github_j.string_of_repository |> Yojson.Basic.from_string |> Yojson.Basic.pretty_to_string)
  | Some (Commit (repo, matched_re)) ->
    sprintf "matched: %s \nfor repo:\n %s\n" matched_re
      (repo |> Github_j.string_of_repository |> Yojson.Basic.from_string |> Yojson.Basic.pretty_to_string)
  | Some (Compare (_, (base, merge))) -> sprintf "matched: for base %s|for merge %s\n" base merge
  | None -> "{None}"

let () =
  List.iter
    (fun (input, expected) ->
      assert (
        let got = gh_link_of_string input in
        match got = expected with
        | true -> true
        | false ->
          Printf.printf "for: %s | expected: %s but got %s" input (gh_link_output expected) (gh_link_output got);
          false
      )
    )
    cases
