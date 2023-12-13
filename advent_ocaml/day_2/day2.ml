open List
open Ast
(* write a parser for the input
   given constraints, a game is possible if each color in a draw is lower than
   the constraints *)

let rec process_file f chan =
  match input_line chan with
  | l ->
      let n = f l in
      n :: process_file f chan
  | exception End_of_file ->
      close_in chan;
      []

let eval_draw_pt1 {red = r ; green = g ; blue = b} rc gc bc =
  r <= rc && g <= gc && b <= bc

let eval_game_pt1 rc gc bc (Ast.Game (i, ds)) =
  let possible =
    List.map (fun d -> eval_draw_pt1 d rc gc bc) ds
    |> List.fold_left ( && ) true
  in
  match possible with true -> i | false -> 0

let parse1 rc gc bc l =
  Lexing.from_string l |> Parser.input Lexer.read |> eval_game_pt1 rc gc bc

let part1 =
  process_file (parse1 12 13 14) (open_in "./input1") |> List.fold_left ( + ) 0

let cube_draw {red = r ; green = g ; blue = b} =
  r * g * b

let eval_game_pt2 (Ast.Game (_, ds)) =
  let m = { red = 0 ; green = 0 ; blue = 0 } in
  let f d1 d2 = 
    { red = Int.max d1.red d2.red ; 
      green = Int.max d1.green d2.green ; 
      blue = Int.max d1.blue d2.blue }
  in
  List.fold_left f m ds |> cube_draw

let parse2 l =
  Lexing.from_string l |> Parser.input Lexer.read |> eval_game_pt2

let part2 = 
  process_file parse2 (open_in "./input1") |> List.fold_left ( + ) 0

let () = 
  print_endline ("Part 1: " ^ string_of_int part1);
  print_endline ("Part 2: " ^ string_of_int part2)
