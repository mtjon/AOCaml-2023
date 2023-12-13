%{
  open Ast
%}

%token <int> GAME
%token <int> RED
%token <int> GREEN
%token <int> BLUE
%token SEP
%token EOF

%start <Ast.game> input

%%

input:
  | i = GAME; ds = separated_nonempty_list(SEP, draw); EOF { Game (i, ds) }

draw:
  | cubes = cubes+ { let m = { red = 0; green = 0; blue = 0 } in 
                 let rec construct d cs = 
                   match cs with
                   | [] -> d
                   | Ast.Red i :: t -> construct {d with red = i } t
                   | Ast.Green i :: t -> construct {d with green = i } t
                   | Ast.Blue i :: t-> construct {d with blue = i } t
                 in
                 construct m cubes }
cubes:
  | i = RED { Red i }
  | i = GREEN { Green i }
  | i = BLUE { Blue i }

