{
open Parser
}


let digit = ['0'-'9']
let noise = ['a'-'z' 'A'-'Z']

rule read =
  parse
  | noise { read lexbuf }
  | digit { DIGIT (int_of_string (Lexing.lexeme lexbuf)) }
  | "one" { ONE }
  | "two" { TWO }
  | "three" { THREE }
  | "four" { FOUR }
  | "five" { FIVE }
  | "six" { SIX }
  | "seven" { SEVEN }
  | "eight" { EIGHT }
  | "nine" { NINE }
  | eof { EOF }
