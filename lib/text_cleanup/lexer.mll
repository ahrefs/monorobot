{}

let digit = ['0' - '9']

let esc = '\027'

let timestamp = esc "_bk;t=" digit* '\007'

let color_code = esc '[' (digit | ';') * ['a'-'z' 'A'-'Z']

let cursor_show_hide = esc "[?25" ('l' | 'h')

let to_delete = timestamp | color_code | cursor_show_hide

rule cleanup buf = parse
| to_delete { cleanup buf lexbuf }
| esc { Buffer.add_string buf "\\027"; cleanup buf lexbuf }
| _ { Buffer.add_string buf (Lexing.lexeme lexbuf); cleanup buf lexbuf }
| eof { () }