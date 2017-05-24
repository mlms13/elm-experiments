module Board exposing (..)

import Array exposing (..)
import Array.Extra exposing (sliceUntil, sliceFrom)

type alias Board a = Array (Cell a)

type Status = InGame
  | Draw
  | Win Player (Int, Int)

type Player = Ex | Oh

type Cell a = Empty | Val a

iconClassForPlayer : Player -> String
iconClassForPlayer player =
  case player of
    Ex -> "fa fa-times ex"
    Oh -> "fa fa-circle-o oh"

size : Int
size = 3

empty : Board a
empty = Array.repeat (size * size) Empty

toRectangle : Board a -> List (List (Cell a))
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


setValueAt : Int -> a -> Board a -> Board a
setValueAt cell val board =
  set cell (Val val) board

check : Board Player -> Status
check board =
  let
    winConH : Int -> Cell a -> Board a -> Maybe (Int, Int)
    winConH offset cell board =
      if length ((filter (\c -> c == cell)) (slice offset (offset + size) board)) == size then
        Just (offset, offset + size)
      else
        Nothing

    allWinCon : Cell a -> Board a -> Maybe (Int, Int)
    allWinCon cell board =
      List.foldl (\off res -> case res of
        Nothing -> winConH (off * size) cell board
        Just _ -> res
      ) Nothing <| List.range 0 size

    toStatus : Player -> Maybe (Int, Int) -> Status
    toStatus player tup =
      case tup of
        Just tup -> Win player tup
        Nothing -> InGame

    checkCells : List Player -> Board Player -> Status
    checkCells list board =
      List.foldl (\player res -> case (player, res) of
        (Val p, InGame) -> toStatus p (allWinCon player board)
        (_, _) -> res
      ) InGame <| List.map Val list
  in
    checkCells [Ex, Oh] board
