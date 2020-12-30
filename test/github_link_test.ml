open Base
open Printf
open Lib
open Github
open Github_t

let mk_repo ?(scheme = "https") prefix prefix_api : repository =
  {
    name = "test_repo";
    full_name = "acme/test_repo";
    url = sprintf "%s://%s/acme/test_repo" scheme prefix;
    commits_url = sprintf "%s://%s/repos/acme/test_repo/commits{/sha}" scheme prefix_api;
    contents_url = sprintf "%s://%s/repos/acme/test_repo/contents/{+path}" scheme prefix_api;
    pulls_url = sprintf "%s://%s/repos/acme/test_repo/pulls{/number}" scheme prefix_api;
    issues_url = sprintf "%s://%s/repos/acme/test_repo/issues{/number}" scheme prefix_api;
  }

let enterprise_repo1 = mk_repo "git.acme.org" "git.acme.org/api/v3"

let enterprise_repo2 = mk_repo "acme.org/path/to/git" "acme.org/path/to/git/api/v3"

let enterprise_repo_insecure = mk_repo ~scheme:"http" "git.acme.org" "git.acme.org/api/v3"

let github_repo = mk_repo "github.com" "api.github.com"

let commit_cases prefix repo =
  [
    sprintf "https://%s/acme/test_repo/commit/69c42640" prefix, Some (Commit (repo, "69c42640"));
    sprintf "https://%s/acme/test_repo/commit/69c42640/" prefix, Some (Commit (repo, "69c42640"));
    sprintf "https://%s/acme/test_repo/commit/69c42640?arg1=123" prefix, Some (Commit (repo, "69c42640"));
    sprintf "https://%s/acme/test_repo/commit/" prefix, None;
    sprintf "https://%s/acme/test_repo/commit" prefix, None;
  ]

let other_cases =
  [
    "http://github.com/acme/test_repo/commit/69c42640", Some (Commit (github_repo, "69c42640"));
    "http://git.acme.org/acme/test_repo/commit/69c42640", Some (Commit (enterprise_repo_insecure, "69c42640"));
    "abc", None;
  ]

let cases =
  List.concat
    [
      commit_cases "github.com" github_repo;
      commit_cases "www.github.com" github_repo;
      commit_cases "git.acme.org" enterprise_repo1;
      commit_cases "acme.org/path/to/git" enterprise_repo2;
      other_cases;
    ]

let () = List.iter cases ~f:(fun (input, expected) -> assert (Poly.equal (gh_link_of_string input) expected))
