module TcTurtle exposing (..)

import Parser exposing (..)


type Inst
    = Inst
        { keyword : String
        , value : Int
        , insts : Maybe (List Inst)
        }


makeSimpleInst : String -> Int -> Inst
makeSimpleInst keyword value =
    Inst
        { keyword = keyword
        , value = value
        , insts = Nothing
        }


makeCompleteInst : String -> Int -> List Inst -> Inst
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
        [ succeed (makeSimpleInst "Forward")
            |. keyword "Forward"
            |. spaces
            |= int
        , succeed (makeSimpleInst "Left")
            |. keyword "Left"
            |. spaces
            |= int
        , succeed (makeSimpleInst "Right")
            |. keyword "Right"
            |. spaces
            |= int
        , succeed (makeCompleteInst "Repeat")
            |. keyword "Repeat"
            |. spaces
            |= int
            |. spaces
            |= lazy (\_ -> parseExpression)
        ]
