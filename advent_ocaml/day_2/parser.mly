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
  | c = cubes+ { Draw c }

cubes:
  | i = RED { Red i }
  | i = GREEN { Green i }
  | i = BLUE { Blue i }

