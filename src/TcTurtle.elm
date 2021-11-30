module TcTurtle exposing (..)

import Parser exposing (int, lazy, run, spaces, succeed, token)


type Inst
    = Forward Int
    | Left Int
    | Right Int
    | Repeat Int (List Inst)


type alias Cursor =
    { x : Float, y : Float, size : Float }
