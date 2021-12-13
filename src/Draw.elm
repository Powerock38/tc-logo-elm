module Draw exposing (..)

import TcTurtle exposing (Inst, Keyword(..))


type alias Cursor =
    { x : Float, y : Float, a : Float }



-- type alias SimpleInst =
--     { keyword : Keyword, value : Float }
-- unfoldRepeats : List Inst -> List SimpleInst
-- unfoldRepeats insts =
--     List.map
--         (\{ keyword, value, repeatInsts } ->
--             case keyword of
--                 Repeat ->
--                     unfoldRepeats repeatInsts
--                 _ ->
--                     { keyword = keyword, value = value }
--         )
--         insts


getLine : Float -> Float -> Float -> Float -> String
getLine x1 y1 x2 y2 =
    "<line x1=\"" ++ String.fromFloat x1 ++ "\" y1=\"" ++ String.fromFloat y1 ++ "\" x2=\"" ++ String.fromFloat x2 ++ "\" y2=\"" ++ String.fromFloat y2 ++ "\" style=\"stroke:red\" />"


drawOne : Cursor -> Inst -> ( String, Cursor )
drawOne c inst =
    let
        { x, y, a } =
            c

        { keyword, value } =
            inst
    in
    case keyword of
        Forward ->
            let
                dx =
                    value * (cos a / 180.0 * pi)

                dy =
                    value * (sin a / 180.0 * pi)
            in
            ( getLine x y (x + dx) (y + dy), Cursor (x + dx) (y + dy) a )

        Left ->
            ( "", Cursor x y (a - value) )

        Right ->
            ( "", Cursor x y (a + value) )

        Repeat ->
            ( "", Cursor x y a )



-- draw : (List Inst) -> String
-- draw =
