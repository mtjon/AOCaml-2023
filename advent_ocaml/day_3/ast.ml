type point = { x : int ; y : int }

type glyphs = 
  | Symbol of point
  | Part of { number : int ; position : point list }

type engine = { symbols : point list; numbers : glyphs list }

