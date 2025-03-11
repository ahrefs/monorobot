let lex lexer string =
  let lexbuf = Lexing.from_string string in
  let buf = Buffer.create (String.length string) in
  lexer buf lexbuf;
  Buffer.contents buf

let cleanup string =
  let string = lex Lexer.cleanup_emoji_choice string in
  let string = lex Lexer.cleanup_crlf string in
  let string = lex Lexer.cleanup_esc_codes string in
  let string = lex Lexer.cleanup_cr string in
  let string = lex Lexer.cleanup_double_lf string in
  string
