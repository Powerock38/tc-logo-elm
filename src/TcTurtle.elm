module TcTurtle exposing (..)

import Parser exposing (..)


type Inst
    = Forward Int
    | Left Int
    | Right Int
    | Repeat Int (List Inst)


type alias Cursor =
    { x : Float, y : Float, size : Float }


read : String -> List Inst
read = run (

    )


parseExpression : Parser (List Inst)
parseExpression = 
    Parser.sequence
    { start = "["
    , separator = ","
    , end = "]"
    , spaces = spaces
    , item = parseInstruction
    , trailing = Trailing.Optional
    }


parseInstruction : Parser (List Inst)
parseInstruction = 
oneOf [
    succeed Forward
    |= symbol "Forward"
    |. spaces
    |= int

    , succeed Left
    |= symbol "Left"
    |. spaces
    |= int

    , succeed Right
    |= symbol "Right"
    |. spaces
    |= int

    , succeed Repeat
    |= symbol "Repeat"
    |. spaces
    |= int
    |. spaces
    |= (\_ -> parseExpression)
]
