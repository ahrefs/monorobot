{}

let digit = ['0' - '9']

let esc = '\027'

let timestamp = esc "_bk;t=" digit* '\007'

let color_code = esc '[' (digit | ';') * ['a'-'z' 'A'-'Z']

let cursor_show_hide = esc "[?25" ('l' | 'h')

let to_delete = timestamp | color_code | cursor_show_hide
let space = ' ' | '\t'

rule cleanup_esc_codes buf = parse
| esc "[K" ([^'\n']*) '\n'
| '\n' ([^'\n']*) ((esc "[1K"))
| '\n' ([^'\n']*) esc "[2K" ([^'\n']*) '\n'
    { Buffer.add_char buf '\n';
      cleanup_esc_codes buf lexbuf }
| to_delete { cleanup_esc_codes buf lexbuf }
| esc
  { Buffer.add_string buf "\\027";
    cleanup_esc_codes buf lexbuf }
| _
  { Buffer.add_string buf (Lexing.lexeme lexbuf);
    cleanup_esc_codes buf lexbuf }
| eof { () }

and cleanup_crlf buf = parse
| ('\r'+) '\n'
  { Buffer.add_char buf '\n';
    cleanup_crlf buf lexbuf }
| _
  { Buffer.add_string buf (Lexing.lexeme lexbuf);
    cleanup_crlf buf lexbuf }
| eof { () }

and cleanup_cr buf = parse
| ((_ # ['\n''\r'])* ) '\r'
    { cleanup_cr buf lexbuf }
| _
  { Buffer.add_string buf (Lexing.lexeme lexbuf);
    cleanup_cr buf lexbuf }
| eof { () }

and cleanup_double_lf buf = parse
| '\n' (space*) ('\n'+)
    { Buffer.add_char buf '\n';
      cleanup_double_lf buf lexbuf }
| _
  { Buffer.add_string buf (Lexing.lexeme lexbuf);
    cleanup_double_lf buf lexbuf }
| eof { () }