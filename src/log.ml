open Printf

let time_to_string ~gmt f =
  let open Unix in
  let t = (if gmt then gmtime else localtime) f in
  let sec = sprintf "%07.4f" (mod_float f 60.) in
  sprintf "%04u-%02u-%02uT%02u:%02u:%s%s" (1900 + t.tm_year) (t.tm_mon + 1) t.tm_mday t.tm_hour t.tm_min sec
    (if gmt then "Z" else "")

let gmt_now_string () = time_to_string ~gmt:true @@ Unix.gettimeofday ()

let line fmt = ksprintf (fun s -> Stdio.print_endline @@ sprintf "%s : %s" (gmt_now_string ()) s) fmt
