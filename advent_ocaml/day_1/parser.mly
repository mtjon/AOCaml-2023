%{
  open Ast
%}

%token <int> DIGIT
%token ONE
%token TWO
%token THREE
%token FOUR
%token FIVE
%token SIX
%token SEVEN
%token EIGHT
%token NINE
%token EOF

%start <Ast.expr list> input

%%

input:
  | e = expr*; EOF { e }

expr:
  | i = DIGIT { Int i }
  | ONE { Int 1 }
  | TWO { Int 2 }
  | THREE { Int 3 }
  | FOUR { Int 4 }
  | FIVE { Int 5 }
  | SIX { Int 6 }
  | SEVEN { Int 7 }
  | EIGHT { Int 8 }
  | NINE { Int 9 }
