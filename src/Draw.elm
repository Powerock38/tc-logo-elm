module Draw exposing (..)

import TcTurtle exposing (Inst(..), Keyword(..), makeCompleteInst)


type alias Cursor =
    { x : Float, y : Float, a : Float }


getLine : Float -> Float -> Float -> Float -> String
getLine x1 y1 x2 y2 =
    "<line x1=\"" ++ String.fromFloat x1 ++ "\" y1=\"" ++ String.fromFloat y1 ++ "\" x2=\"" ++ String.fromFloat x2 ++ "\" y2=\"" ++ String.fromFloat y2 ++ "\" style=\"stroke:red\" />"


changeAngle : Float -> Float
changeAngle a =
    if a < 0 then
        changeAngle a + 360

    else if a >= 360 then
        changeAngle a - 360

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


draw : List Inst -> Cursor -> String
draw insts c =
    case insts of
        [] ->
            ""

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
                            newRepeat =
                                makeCompleteInst Repeat (value - 1) is
                        in
                        draw (newRepeat :: is) c ++ draw is c

                    else
                        draw is c

                Forward ->
                    let
                        dx =
                            value * (cos a / 180.0 * pi)

                        dy =
                            value * (sin a / 180.0 * pi)
                    in
                    getLine x y (x + dx) (y + dy)
                        ++ draw is
                            { x = x + dx, y = y + dy, a = a }

                Left ->
                    draw is
                        { x = x, y = y, a = changeAngle (a - value) }

                Right ->
                    draw is
                        { x = x, y = y, a = changeAngle (a + value) }
