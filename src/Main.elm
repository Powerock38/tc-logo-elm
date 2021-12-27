module Main exposing (..)

import Browser
import Draw exposing (Cursor, draw)
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput)
import Svg exposing (svg)
import Svg.Attributes exposing (height, width, viewBox)
import TcTurtle exposing (read)
import Html.Attributes exposing (style)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { inputCommand : String
    }


init : Model
init =
    { inputCommand = "" }



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { model | inputCommand = newContent }


execute : String -> List (Html msg)
execute command =
    case read command of
        Ok insts ->
            draw insts (Cursor 0 0 0)

        Err deadends ->
            [ text (Debug.toString deadends) ]



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "TcTurtle instructions. Example : [Repeat 8 [Forward 100, Left 45]]", value model.inputCommand, onInput Change, style "width" "100%", style "height" "3rem" ] []
        , div []
            [ svg [ width "1000", height "800", viewBox "-500 -400 1000 800" ] (execute model.inputCommand)
            ]
        ]
