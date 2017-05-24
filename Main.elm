module Main exposing (main)

import Html exposing (Html, button, div, text, table, tr, td, p)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List exposing (map, indexedMap)
import Board exposing (..)


main : Program Never Model Msg
main =
  Html.program
    { init = (model, Cmd.none)
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }


-- MODEL


switchPlayer : Player -> Player
switchPlayer player =
  case player of
    Ex -> Oh
    Oh -> Ex

type alias Model =
  { board : Board Player
  , currentPlayer : Player
  , gameStatus : Status
  }

model : Model
model =
  { board = Board.empty
  , currentPlayer = Ex
  , gameStatus = InGame
  }


-- UPDATE

type Msg = Set Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Set index ->
      let
        newBoard = setValueAt index model.currentPlayer model.board
      in
        (
          { currentPlayer = switchPlayer model.currentPlayer
          , board = newBoard
          , gameStatus = check newBoard
          }
          , Cmd.none
        )


-- VIEW

renderCell : Int -> Cell Player -> Html Msg
renderCell index cell =
  case cell of
    Empty ->
      div [onClick (Set index)] []
    Val v ->
      div []
        [ Html.i [ class <| iconClassForPlayer v ] []]


renderBoard : (Int -> Cell Player -> Html msg) -> Board Player -> Html msg
renderBoard f board =
  let
    rectangle : List (List (Cell Player))
    rectangle = toRectangle board

    renderCell : Int -> Cell Player -> Html msg
    renderCell index a =
      td [] [f index a]

    renderRow : Int -> List (Cell Player) -> Html msg
    renderRow row arr =
      tr [] (indexedMap (\i c -> renderCell (i + (row * Board.size)) c) arr)
  in
    table [] (indexedMap renderRow rectangle)

renderPlayer : Player -> Html msg
renderPlayer player =
  let
    playerName =
      case player of
        Ex -> "ex"
        Oh -> "oh"
  in
    p [] [text ("current player: " ++ playerName)]

renderStatus : Status -> Html msg
renderStatus status =
  let
    s =
      case status of
        Draw -> "Draw"
        InGame -> "In game"
        Win Ex _ -> "Ex wins!"
        Win Oh _ -> "Oh wins!"
  in
    p [] [text ("Status: " ++ s)]

view : Model -> Html Msg
view model =
  div []
    [ renderBoard renderCell model.board
    , renderPlayer model.currentPlayer
    , renderStatus model.gameStatus
    ]
