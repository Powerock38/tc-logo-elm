module Main exposing (..)

import Browser
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

import TcTurtle exposing (read)


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



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "TcTurtle instructions", value model.inputCommand, onInput Change ] []
        , div [] [ text (Debug.toString (read model.inputCommand)) ]
        ]
