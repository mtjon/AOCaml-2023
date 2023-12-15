{
open Parser
}


let noise = ['.']
let integer = ['0'-'9']+
let symbol = [^'.' '0'-'9']

rule read =
  parse
  | noise { read lexbuf }
  | symbol { SYMBOL (Lexing.lexeme_start lexbuf) }
  | integer { PART (int_of_string (Lexing.lexeme lexbuf), (Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf)) }
  | eof { EOF }

