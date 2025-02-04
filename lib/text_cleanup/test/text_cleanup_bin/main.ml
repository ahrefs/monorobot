let () =
  In_channel.with_open_bin Sys.argv.(1) (fun file ->
      file |> In_channel.input_all |> Text_cleanup.cleanup |> Printf.printf "%S")
