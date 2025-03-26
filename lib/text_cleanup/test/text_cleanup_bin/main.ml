let () =
  In_channel.with_open_bin Sys.argv.(1) (fun file ->
      let content = In_channel.input_all file in
      let content =
        if Array.length Sys.argv = 3 && Sys.argv.(2) = "--json" then
          (Monorobotlib.Buildkite_j.job_log_of_string content).content
        else content
      in

      content |> Text_cleanup.cleanup |> Printf.printf "%S")
