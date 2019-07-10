open Httpaf
open Base
open Lwt.Infix

let read_body response_body =
  let open Httpaf in
  let buf = Buffer.create 0x2000 in
  let body_read, notify_body_read = Lwt.wait () in
  let rec read_fn () =
    Body.schedule_read response_body
      ~on_eof:(fun () ->
        Body.close_reader response_body;
        Lwt.wakeup_later notify_body_read (Buffer.contents buf))
      ~on_read:(fun response_fragment ~off ~len ->
        let response_fragment_bytes = Bytes.create len in
        Lwt_bytes.blit_to_bytes response_fragment off response_fragment_bytes 0 len;
        Buffer.add_bytes buf response_fragment_bytes;
        read_fn ())
  in
  read_fn ();
  body_read

let request_handler (_ : Unix.sockaddr) (reqd : Httpaf.Reqd.t) =
  (let { Request.meth; _ } = Reqd.request reqd in
   match meth with
   | `POST ->
     read_body (Reqd.request_body reqd) >|= fun body ->
     Stdio.Out_channel.print_endline body;
     let headers = Headers.of_list [ "Content-Length", "0" ] in
     Reqd.respond_with_string reqd (Response.create ~headers `OK) ""
   | meth ->
     let response_body = Printf.sprintf "%s is not an allowed method\n" (Method.to_string meth) in
     let headers = Headers.of_list [ "Content-Length", Int.to_string (String.length response_body) ] in
     Reqd.respond_with_string reqd (Response.create ~headers `Method_not_allowed) response_body;
     Lwt.return_unit)
  |> ignore
