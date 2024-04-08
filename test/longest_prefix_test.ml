open Lib
open Github_t
open Slack_message

let make_test_file filename =
  {
    sha = "something something";
    filename;
    status = "something something";
    additions = 5;
    deletions = 5;
    changes = 10;
    url = "test_link";
  }

let single_file = [ make_test_file "testdir1/testdir2/changed_file.txt" ]

let multiple_files_same_dir =
  List.map make_test_file
    [
      "testdir1/testdir2/changed_file2.txt";
      "testdir1/testdir2/changed_file3.txt";
      "testdir1/testdir2/changed_file1.txt";
    ]

let multiple_files_common_root_dir =
  List.map make_test_file [ "testdir1/changed_file2.txt"; "backend/changed_file3.txt"; "changed_file1.txt" ]

let multiple_files_common_dir =
  List.map make_test_file
    [ "backend/something/changed_file2.txt"; "backend/something/changed_file3.txt"; "backend/some/changed_file1.txt" ]

let () =
  assert (condense_file_changes single_file = "_modified `testdir1/testdir2/changed_file.txt` (+5-5)_");
  assert (condense_file_changes multiple_files_same_dir = "modified 3 files in `testdir1/testdir2/`");
  assert (condense_file_changes multiple_files_common_root_dir = "modified 3 files");
  assert (condense_file_changes multiple_files_common_dir = "modified 3 files in `backend/`")
