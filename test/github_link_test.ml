open Base
open Printf
open Lib
open Github
open Github_t

let mk_repo ?(scheme = "https") prefix prefix_api : repository =
  {
    name = "monorepo";
    full_name = "ahrefs/monorepo";
    url = sprintf "%s://%s/ahrefs/monorepo" scheme prefix;
    commits_url = sprintf "%s://%s/repos/ahrefs/monorepo/commits{/sha}" scheme prefix_api;
    contents_url = sprintf "%s://%s/repos/ahrefs/monorepo/contents/{+path}" scheme prefix_api;
    pulls_url = sprintf "%s://%s/repos/ahrefs/monorepo/pulls{/number}" scheme prefix_api;
    issues_url = sprintf "%s://%s/repos/ahrefs/monorepo/issues{/number}" scheme prefix_api;
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
    sprintf "https://%s/ahrefs/monorepo/commit/69c42640" prefix, Some (Commit (repo, "69c42640"));
    sprintf "https://%s/ahrefs/monorepo/commit/69c42640/" prefix, Some (Commit (repo, "69c42640"));
    sprintf "https://%s/ahrefs/monorepo/commit/69c42640?arg1=123" prefix, Some (Commit (repo, "69c42640"));
    sprintf "https://%s/ahrefs/monorepo/commit/" prefix, None;
    sprintf "https://%s/ahrefs/monorepo/commit" prefix, None;
  ]

let compare_cases prefix repo =
  [
    sprintf "https://%s/ahrefs/monorepo/compare/master...develop" prefix, Some (Compare (repo, "master...develop"));
    sprintf "https://%s/ahrefs/monorepo/compare/develop...master/" prefix, Some (Compare (repo, "develop...master"));
    ( sprintf "https://%s/ahrefs/monorepo/compare/master...sewen/one-feature" prefix,
      Some (Compare (repo, "master...sewen/one-feature")) );
    ( sprintf "https://%s/ahrefs/monorepo/compare/master...sewen/one-feature/" prefix,
      Some (Compare (repo, "master...sewen/one-feature")) );
    ( sprintf "https://%s/ahrefs/monorepo/compare/sewen/one-feature...master/" prefix,
      Some (Compare (repo, "sewen/one-feature...master")) );
    sprintf "https://%s/ahrefs/monorepo/compare" prefix, None;
  ]

let other_cases =
  [
    "http://github.com/ahrefs/monorepo/commit/69c42640", Some (Commit (github_repo, "69c42640"));
    "http://example.org/ahrefs/monorepo/commit/69c42640", Some (Commit (enterprise_repo_insecure, "69c42640"));
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
      compare_cases "www.github.com" github_repo;
      pr_cases "example.org" enterprise_repo1;
      issue_cases "example.org" enterprise_repo1;
      commit_cases "example.org" enterprise_repo1;
      pr_cases "example.org/path/to/git" enterprise_repo2;
      issue_cases "example.org/path/to/git" enterprise_repo2;
      commit_cases "example.org/path/to/git" enterprise_repo2;
      compare_cases "example.org/path/to/git" enterprise_repo2;
      other_cases;
    ]

let () =
  List.iter cases ~f:(fun (input, expected) ->
    assert (
      match Poly.equal (gh_link_of_string input) expected with
      | true -> true
      | false ->
        Stdio.print_endline input;
        false
    )
  )
