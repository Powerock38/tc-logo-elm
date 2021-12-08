module TcTurtle exposing (..)

import Parser exposing (..)

type Inst
    = Forward Int
    | Left Int
    | Right Int
    | Repeat Int (List Inst)


type alias Cursor =
    { x : Float, y : Float, size : Float }


read : String -> Result (List DeadEnd) (List Inst)
read = run parseExpression 


parseExpression : Parser (List Inst)
parseExpression = 
    Parser.sequence
    { start = "["
    , separator = ","
    , end = "]"
    , spaces = spaces
    , item = parseInstruction
    , trailing = Parser.Optional
    }


parseInstruction : Parser Inst
parseInstruction = 
    oneOf [
        succeed Forward
        |= keyword "Forward"
        |. spaces
        |= int

        , succeed Left
        |= keyword "Left"
        |. spaces
        |= int

        , succeed Right
        |= keyword "Right"
        |. spaces
        |= int

        , succeed Repeat
        |= keyword "Repeat"
        |. spaces
        |= int
        |. spaces
        |= (\_ -> parseExpression)
    ]
