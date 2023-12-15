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

(* Could solve in processing symbols to get hitboxes, then process part numbers
   Could also get hitboxes straight from Parser, then travers list once
*)

let construct_engine gl =
  let expand_hitboxes p =
    let cartesian l1 l2 = 
      List.flatten ( List.map (fun e -> List.map (fun e' -> { x = e; y = e'}) l2) l1 )
    in
    let xs = List.init 3 (fun x -> x + (p.x - 1)) in
    let ys = List.init 3 (fun y -> y + (p.y - 1)) in
    cartesian xs ys
  in
  let rec aux e gl = 
    match gl with
    | [] -> e
    | Ast.Symbol s :: t-> aux { e with symbols = List.append (expand_hitboxes s) e.symbols } t
    | Ast.Part _ :: t -> aux { e with numbers = e.numbers } t
  in
  aux { symbols = []; numbers = [] } gl

let eval_p1 gl =
  let engine = construct_engine gl in
  let rec pps ps = 
    match ps with
    | [] -> ""
    | h :: t -> "{ x : " ^ string_of_int h.x ^ "; y : " ^ string_of_int h.y ^ " }" ^ pps t
  in
  let get_positions g =
    match g with
    | Ast.Part { position = ps ; _} -> ps
    | _ -> []
  in
  print_endline ("part no. on: " ^ pps ( List.flatten (List.map get_positions engine.numbers )) ^ " symbols on: " ^ pps engine.symbols );
  0

let parse1 l = Lexing.from_string l |> Parser.input Lexer.read |> eval_p1

let part1 =
  process_file parse1 (open_in "./input1")
  |> List.fold_left ( + ) 0

let () =
  print_endline "Hello, Day 3!";
  print_endline ("Part 1: " ^ string_of_int part1)


