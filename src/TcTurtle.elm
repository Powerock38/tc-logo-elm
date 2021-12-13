module TcTurtle exposing (..)

import Parser exposing (..)

type Keyword = Forward | Left | Right | Repeat

type Inst
    = Inst
        { keyword : Keyword
        , value : Float
        , insts : Maybe (List Inst)
        }


makeSimpleInst : Keyword -> Float -> Inst
makeSimpleInst keyword value =
    Inst
        { keyword = keyword
        , value = value
        , insts = Nothing
        }


makeCompleteInst : Keyword -> Float -> List Inst -> Inst
makeCompleteInst keyword value insts =
    Inst
        { keyword = keyword
        , value = value
        , insts = Just insts
        }


read : String -> Result (List DeadEnd) (List Inst)
read =
    run parseExpression


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
    oneOf
        [ succeed (makeSimpleInst Forward)
            |. keyword "Forward"
            |. spaces
            |= float
        , succeed (makeSimpleInst Left)
            |. keyword "Left"
            |. spaces
            |= float
        , succeed (makeSimpleInst Right)
            |. keyword "Right"
            |. spaces
            |= float
        , succeed (makeCompleteInst Repeat)
            |. keyword "Repeat"
            |. spaces
            |= float
            |. spaces
            |= lazy (\_ -> parseExpression)
        ]
