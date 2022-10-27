open Base
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
    branches_url = sprintf "%s://%s/repos/%s/monorepo/branches{/branch}" scheme prefix_api owner;
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
    sprintf "https://%s/ahrefs/monorepo/commit/" prefix, None;
    sprintf "https://%s/ahrefs/monorepo/commit" prefix, None;
  ]

let compare_cases prefix repo prefix_api =
  [
    ( sprintf "https://%s/ahrefs/monorepo/compare/master...develop" prefix,
      Some (Compare (repo, "master...develop", (repo, "master"), (repo, "develop"))) );
    ( sprintf "https://%s/ahrefs/monorepo/compare/develop...master/" prefix,
      Some (Compare (repo, "develop...master", (repo, "develop"), (repo, "master"))) );
    ( sprintf "https://%s/ahrefs/monorepo/compare/master...sewen/one-feature" prefix,
      Some (Compare (repo, "master...sewen/one-feature", (repo, "master"), (repo, "sewen/one-feature"))) );
    ( sprintf "https://%s/ahrefs/monorepo/compare/master...sewen/one-feature/" prefix,
      Some (Compare (repo, "master...sewen/one-feature", (repo, "master"), (repo, "sewen/one-feature"))) );
    ( sprintf "https://%s/ahrefs/monorepo/compare/sewen/one-feature...master/" prefix,
      Some (Compare (repo, "sewen/one-feature...master", (repo, "sewen/one-feature"), (repo, "master"))) );
    ( sprintf "https://%s/ahrefs/monorepo/compare/sewen/one-feature...other:master/" prefix,
      Some
        (Compare
           ( repo,
             "sewen/one-feature...other:master",
             (repo, "sewen/one-feature"),
             (mk_repo ~owner:"other" prefix prefix_api, "master")
           )
        ) );
     ( sprintf "https://%s/ahrefs/monorepo/compare/master...other:monorobot:119-unfurling-commit-range-links/" prefix,
      Some
        (Compare
           ( repo,
             "master...other:monorobot:119-unfurling-commit-range-links",
             (repo, "master"),
             (mk_repo ~owner:"other" prefix prefix_api, "119-unfurling-commit-range-links")
           )
        ) );
    ( sprintf "https://%s/ahrefs/monorepo/compare/other:1-feature...master/" prefix,
      Some
        (Compare
           (repo, "other:1-feature...master", (mk_repo ~owner:"other" prefix prefix_api, "1-feature"), (repo, "master"))
        ) );
    ( sprintf "https://%s/ahrefs/monorepo/compare/sewen/123_x-feature...master" prefix,
      Some (Compare (repo, "sewen/123_x-feature...master", (repo, "sewen/123_x-feature"), (repo, "master"))) );
    sprintf "https://%s/ahrefs/monorepo/compare" prefix, None;
  ]

let other_cases =
  [
    "http://github.com/ahrefs/monorepo/commit/69c42640", None;
    "http://example.org/ahrefs/monorepo/commit/69c42640", None;
    "abc", None;
  ]

let cases =
  List.concat
    [
      pr_cases "github.com" github_repo;
      issue_cases "github.com" github_repo;
      commit_cases "github.com" github_repo;
      pr_cases "www.github.com" github_repo;
      issue_cases "www.github.com" github_repo;
      commit_cases "www.github.com" github_repo;
      compare_cases "github.com" github_repo "api.github.com";
      pr_cases "example.org" enterprise_repo1;
      issue_cases "example.org" enterprise_repo1;
      commit_cases "example.org" enterprise_repo1;
      compare_cases "example.org" enterprise_repo1 "example.org/api/v3";
      pr_cases "example.org/path/to/git" enterprise_repo2;
      issue_cases "example.org/path/to/git" enterprise_repo2;
      commit_cases "example.org/path/to/git" enterprise_repo2;
      compare_cases "example.org/path/to/git" enterprise_repo2 "example.org/path/to/git/api/v3";
      other_cases;
    ]

let gh_link_output = function
  | Some (Issue (repo, matched_re)) | Some (Pull_request (repo, matched_re)) ->
    sprintf "matched: %d \nfor repo %s\n" matched_re
      (repo |> Github_j.string_of_repository |> Yojson.Basic.from_string |> Yojson.Basic.pretty_to_string)
  | Some (Commit (repo, matched_re)) ->
    sprintf "matched: %s \nfor repo:\n %s\n" matched_re
      (repo |> Github_j.string_of_repository |> Yojson.Basic.from_string |> Yojson.Basic.pretty_to_string)
  | Some (Compare (_, matched_re, (base, base_branch), (merge, merge_branch))) ->
    sprintf "matched: %s \nfor base branch %s in repo\n %s\nfor merge branch %s in repo: %s\n" matched_re base_branch
      (base |> Github_j.string_of_repository |> Yojson.Basic.from_string |> Yojson.Basic.pretty_to_string)
      merge_branch
      (merge |> Github_j.string_of_repository |> Yojson.Basic.from_string |> Yojson.Basic.pretty_to_string)
  | None -> "{None}"

let () =
  List.iter cases ~f:(fun (input, expected) ->
    assert (
      let got = gh_link_of_string input in
      match Poly.equal got expected with
      | true -> true
      | false ->
        Stdio.printf "for: %s | expected: %s but got %s" input (gh_link_output expected) (gh_link_output got);
        false
    )
  )
