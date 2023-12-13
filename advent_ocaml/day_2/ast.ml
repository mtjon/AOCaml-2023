type cube =
  | Red of int
  | Green of int
  | Blue of int

type draw = 
  | Draw of cube list

type game =
  | Game of (int * draw list)
