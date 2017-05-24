module Main exposing (main)

import Html exposing (Html, button, div, text, table, tr, td, p)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List exposing (map, indexedMap)
import Board exposing (..)
import Cell exposing (..)


main : Program Never Model Msg
main =
  Html.program
    { init = (model, Cmd.none)
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }


-- MODEL

type Player = Ex | Oh

switchPlayer : Player -> Player
switchPlayer player =
  case player of
    Ex -> Oh
    Oh -> Ex

type alias Model =
  { board : Board Cell
  , currentPlayer : Player
  }

model : Model
model =
  { board = fill Empty
  , currentPlayer = Ex
  }


-- UPDATE

type Msg = Set Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Set index ->
      let cell : Cell
          cell = case model.currentPlayer of
            Ex -> X
            Oh -> O
      in
        (
          { currentPlayer = switchPlayer model.currentPlayer
          , board = setAt index cell model.board
          }
          , Cmd.none
        )


-- VIEW

renderCell : Int -> Cell -> Html Msg
renderCell index cell =
  case cell of
    Empty ->
      div [onClick (Set index)] []
    _ ->
      div []
        [ Html.i [ class <| iconClass cell ] []]


renderBoard : Board a -> (Int -> a -> Html msg) -> Html msg
renderBoard board f =
  let
    rectangle : List (List a)
    rectangle = toRectangle board

    renderCell : Int -> a -> Html msg
    renderCell index a =
      td [] [f index a]

    renderRow : Int -> List a -> Html msg
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

view : Model -> Html Msg
view model =
  div []
    [ renderBoard model.board renderCell
    , renderPlayer model.currentPlayer
    ]
