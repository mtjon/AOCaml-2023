type cube =
  | Red of int
  | Green of int
  | Blue of int

type draw = { red: int; green: int; blue: int }

type game =
  | Game of (int * draw list)
