{}

let digit = ['0' - '9']

let esc = '\027'

let timestamp = esc "_bk;t=" digit* '\007'

let color_code = esc '[' (digit | ';') * ['a'-'z' 'A'-'Z']

let to_delete = timestamp | color_code

rule cleanup buf = parse
| to_delete { cleanup buf lexbuf }
| _ { Buffer.add_string buf (Lexing.lexeme lexbuf); cleanup buf lexbuf }
| eof { () }