{
open Parser
}

let ws = [' ']
let integer = ['0'-'9']+

rule read =
  parse
  | ws { read lexbuf }
  | "Game " integer { GAME (int_of_string (Lexing.lexeme lexbuf)) }

