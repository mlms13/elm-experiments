module Cell exposing (..)

type Cell = X | O | Empty

iconClass : Cell -> String
iconClass cell =
  case cell of
    Empty -> ""
    X -> "fa fa-times ex"
    O -> "fa fa-circle-o oh"
