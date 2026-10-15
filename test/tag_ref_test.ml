open Monorobotlib

let check name expected actual =
  let show = function
    | None -> "none"
    | Some s -> s
  in
  if String.equal (show expected) (show actual) then Printf.printf "PASS %s\n" name
  else begin
    Printf.printf "FAIL %s: expected %s, got %s\n" name (show expected) (show actual);
    exit 1
  end

let () =
  check "annotated release tag" (Some "release-20260930-0951") (Github.tag_of_ref "refs/tags/release-20260930-0951");
  check "tag name containing slashes" (Some "v1/2") (Github.tag_of_ref "refs/tags/v1/2");
  check "branch ref" None (Github.tag_of_ref "refs/heads/develop");
  check "bare name" None (Github.tag_of_ref "develop")
