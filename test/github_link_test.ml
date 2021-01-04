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
  }

let enterprise_repo1 = mk_repo "example.org" "example.org/api/v3"

let enterprise_repo2 = mk_repo "example.org/path/to/git" "example.org/path/to/git/api/v3"

let enterprise_repo_insecure = mk_repo ~scheme:"http" "example.org" "example.org/api/v3"

let github_repo = mk_repo "github.com" "api.github.com"

let commit_cases prefix repo =
  [
    sprintf "https://%s/ahrefs/monorepo/commit/69c42640" prefix, Some (Commit (repo, "69c42640"));
    sprintf "https://%s/ahrefs/monorepo/commit/69c42640/" prefix, Some (Commit (repo, "69c42640"));
    sprintf "https://%s/ahrefs/monorepo/commit/69c42640?arg1=123" prefix, Some (Commit (repo, "69c42640"));
    sprintf "https://%s/ahrefs/monorepo/commit/" prefix, None;
    sprintf "https://%s/ahrefs/monorepo/commit" prefix, None;
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
      commit_cases "github.com" github_repo;
      commit_cases "www.github.com" github_repo;
      commit_cases "example.org" enterprise_repo1;
      commit_cases "example.org/path/to/git" enterprise_repo2;
      other_cases;
    ]

let () = List.iter cases ~f:(fun (input, expected) -> assert (Poly.equal (gh_link_of_string input) expected))
