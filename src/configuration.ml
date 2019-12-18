
module Curl = struct
  let () = Curl.global_init Curl.CURLINIT_GLOBALALL

  let writer_callback a d =
    Buffer.add_string a d;
    String.length d

  let init_conn url =
    let r = Buffer.create 16384
    and c = Curl.init () in
    Curl.set_timeout c 1200;
    Curl.set_sslverifypeer c false;
    Curl.set_sslverifyhost c Curl.SSLVERIFYHOST_EXISTENCE;
    Curl.set_writefunction c (writer_callback r);
    Curl.set_tcpnodelay c true;
    Curl.set_verbose c false;
    Curl.set_url c url;
    r, c
end
