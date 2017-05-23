module Board exposing (..)

import Array exposing (..)
import Array.Extra exposing (sliceUntil, sliceFrom)

type alias Board a = Array a

size : Int
size = 3

fill : a -> Board a
fill a = Array.repeat (size * size) a

toRectangle : Board a -> List (List a)
toRectangle board =
  case length board < size of
    True ->
      []
    False ->
      let
        h = toList (sliceUntil size board)
        t = sliceFrom size board
      in
        [h] ++ (toRectangle t)


setAt : Int -> a -> Board a -> Board a
setAt cell val board =
  set cell val board
