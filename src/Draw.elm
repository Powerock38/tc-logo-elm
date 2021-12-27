module Draw exposing (..)

import Svg exposing (Svg, line)
import Svg.Attributes exposing (..)
import TcTurtle exposing (Inst(..), Keyword(..), makeCompleteInst)


type alias Cursor =
    { x : Float, y : Float, a : Float }


getLine : Float -> Float -> Float -> Float -> Svg msg
getLine x1_p y1_p x2_p y2_p =
    line
        [ x1 (String.fromFloat x1_p)
        , y1
            (String.fromFloat y1_p)
        , x2
            (String.fromFloat x2_p)
        , y2
            (String.fromFloat y2_p)
        , style
            "stroke: red"
        ]
        []


changeAngle : Float -> Float
changeAngle a =
    if a < 0 then
        changeAngle (a + 360)

    else if a >= 360 then
        changeAngle (a - 360)

    else
        a


getKeyword : Inst -> Keyword
getKeyword inst =
    case inst of
        Inst i ->
            i.keyword


getValue : Inst -> Float
getValue inst =
    case inst of
        Inst i ->
            i.value


getInsts : Inst -> List Inst
getInsts inst =
    case inst of
        Inst i ->
            let
                is_m =
                    i.insts
            in
            case is_m of
                Just is ->
                    is

                Nothing ->
                    []


draw : List Inst -> Cursor -> List (Svg msg)
draw insts c =
    case insts of
        [] ->
            []

        inst :: is ->
            let
                { x, y, a } =
                    c

                keyword =
                    getKeyword inst

                value =
                    getValue inst
            in
            case keyword of
                Repeat ->
                    if value > 0 then
                        let
                            inst_insts =
                                getInsts inst

                            newRepeat =
                                makeCompleteInst Repeat (value - 1) inst_insts
                        in
                        draw (inst_insts ++ [ newRepeat ]) c ++ draw is c

                    else
                        draw is c

                Forward ->
                    let
                        dx =
                            value * cos (degrees a)

                        dy =
                            value * sin (degrees a)
                    in
                    getLine x y (x + dx) (y + dy)
                        :: draw is
                            { x = x + dx, y = y + dy, a = a }

                Left ->
                    draw is
                        { x = x, y = y, a = changeAngle (a - value) }

                Right ->
                    draw is
                        { x = x, y = y, a = changeAngle (a + value) }
