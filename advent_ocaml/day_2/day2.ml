open List
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

let eval_draw (Ast.Draw cubes) rc gc bc =
  let f c =
    match c with
    | Ast.Red i -> i <= rc
    | Ast.Green i -> i <= gc
    | Ast.Blue i -> i <= bc
  in
  List.for_all f cubes

let eval_game rc gc bc (Ast.Game (i, ds)) =
  let possible =
    List.map (fun d -> eval_draw d rc gc bc) ds |> List.fold_left ( && ) true
  in
  match possible with true -> i | false -> 0

let parse1 rc gc bc l =
  Lexing.from_string l |> Parser.input Lexer.read |> eval_game rc gc bc

let part1 =
  process_file (parse1 12 13 14) (open_in "./input1") |> List.fold_left (+) 0

let () =
  print_endline ("Part 1: " ^ string_of_int part1)
