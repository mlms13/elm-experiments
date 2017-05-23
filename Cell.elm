module Cell exposing (..)

type Cell = X | O | Empty

show : Cell -> String
show cell =
  case cell of
    Empty -> "-"
    X -> "x"
    O -> "o"
