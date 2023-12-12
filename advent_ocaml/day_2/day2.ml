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

let parse1 l =
  (*Lexing.from_string l |> Parser.input Lexer.read *)
  String.length l

let part1 = 
  process_file parse1 (open_in "./input1") |> List.fold_left (+) 0

let () =
  print_endline "Hello, Day2";
  print_endline ("Part 1: " ^ string_of_int part1);
