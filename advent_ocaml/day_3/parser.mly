%{
  open Ast
%}

%token <int> SYMBOL
%token <int * int * int> PART
%token EOF

%start <Ast.glyphs list> input

%%

input:
  | e = glyph+ ; EOF { e }

glyph:
  | sx = SYMBOL { Symbol { x = sx ; y = 0 } }
  | p = PART { match p with 
               | (i, s, e) -> let ps = List.init (e-s) (fun x -> x + s) 
                                       |> List.map (fun x -> {x = x; y = 0})
                              in Part { number = i ; position = ps } }
