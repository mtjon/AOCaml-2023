open List
open Advent_ocaml
open Str

(** [parse_line line] is the concatenation of first and last digit in [line]. *)
let parse_line line =
  let is_digit = function '0' .. '9' -> true | _ -> false in
  String.to_seq line |> List.of_seq |> filter is_digit |> fun cs ->
  (hd cs, hd (rev cs)) |> fun (f, l) ->
  int_of_string (String.make 1 f ^ String.make 1 l)

(** [parse_line_menhir line] is the concatenation of first and last (spelled out)
    digit in [line]. *)
let parse_line_menhir line =
  let construct_int cs =
    let f = hd cs |> fun (Ast.Int i) -> i in
    let l = hd (rev cs) |> fun (Ast.Int i) -> i in
    int_of_string (string_of_int f ^ string_of_int l)
  in
  Lexing.from_string line |> Parser.input Lexer.read |> construct_int

let parse_p2 line =
  let re =
    Str.regexp
      "[1-9]\\|one\\|two\\|three\\|four\\|five\\|six\\|seven\\|eight\\|nine"
  in
  let parse_digit s =
    match s with
    | "one" -> "1"
    | "two" -> "2"
    | "three" -> "3"
    | "four" -> "4"
    | "five" -> "5"
    | "six" -> "6"
    | "seven" -> "7"
    | "eight" -> "8"
    | "nine" -> "9"
    | _ -> s
  in
  let extract_fst l =
    let _ = Str.search_forward re line 0 in
    parse_digit (Str.matched_string l)
  in
  let extract_lst l =
    let _ = Str.search_backward re line (String.length line) in
    parse_digit (Str.matched_string l)
  in
  let f = extract_fst line in
  let l = extract_lst line in
  int_of_string (f ^ l)

let rec process_file f chan =
  match input_line chan with
  | l ->
      let n = f l in
      n :: process_file f chan
  | exception End_of_file ->
      close_in chan;
      []

(** [part_1 file] is the answer to Day 1 Part 1, where [file] is the puzzle
    input *)
let part_1 file =
  process_file parse_line (open_in file) |> List.fold_left ( + ) 0

(** [part_2 file] is the answer to Day 1 Part 2, where [file] is the puzzle
    input *)
let part_2 file = process_file parse_p2 (open_in file) |> List.fold_left ( + ) 0

(** [part_lr1 file] would be the answer to Day 1 Part 2, except it doesn't
    support overlapping spelled-out digits, like "oneight". [file] is the
    puzzle input *)
let part_lr1 file =
  process_file parse_line_menhir (open_in file) |> List.fold_left ( + ) 0

let () =
  let p1 = part_1 "./input1" |> string_of_int in
  print_endline ("Part 1: " ^ p1);

  let p2 = part_2 "./input2" in
  print_endline ("Part 2: " ^ string_of_int p2);

  let lr1_parser = part_lr1 "./input2" in
  print_endline ("Part lr(1):" ^ string_of_int lr1_parser);

  let test = parse_p2 "aoneeight1arstarst398oneight" in
  print_endline ("test: " ^ string_of_int test)
