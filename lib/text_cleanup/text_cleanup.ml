let cleanup string =
  let lexbuf = Lexing.from_string string in
  let buf = Buffer.create 16 in
  Lexer.cleanup buf lexbuf;
  Buffer.contents buf
