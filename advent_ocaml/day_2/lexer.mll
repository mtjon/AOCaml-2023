{
open Parser
}


let ws = [' ']
let noise = [':' ',']
let integer = ['0'-'9']+

rule read =
  parse
  | ws { read lexbuf }
  | noise { read lexbuf }
  | "Game" ws+ (integer as i) { GAME (int_of_string i) }
  | (integer as i) ws+ "red" { RED (int_of_string i) }
  | (integer as i) ws+ "green" { GREEN (int_of_string i) }
  | (integer as i) ws+ "blue" { BLUE (int_of_string i) }
  | ";" { SEP }
  | eof { EOF }

