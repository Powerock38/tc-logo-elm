module Main exposing (..)

import Browser
import Draw exposing (Cursor, draw)
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import TcTurtle exposing (read)
import Html.Parser
import Html.Parser.Util


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


textHtml : String -> List (Html.Html msg)
textHtml t =
    case Html.Parser.run t of
        Ok nodes ->
            Html.Parser.Util.toVirtualDom nodes

        Err _ ->
            []

execute : String -> String
execute command =
    case read command of
        Ok insts ->
            draw insts (Cursor 0 0 0)

        Err deadends ->
            Debug.toString deadends



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "TcTurtle instructions", value model.inputCommand, onInput Change ] []
        , div [] [ textHtml (execute model.inputCommand) ]
        ]
