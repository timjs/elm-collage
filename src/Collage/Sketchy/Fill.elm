module Collage.Sketchy.Fill exposing (hachureLines)

import Collage exposing (..)
import Collage.Core as Core exposing (FillStyle(..))
import Helpers.List
import Color exposing (..)



-- BASED ON: https://github.com/rough-stuff/rough/blob/e9b0fdf36952a7a0f02e8015f4abac1ad39981c5/src/fillers/scan-line-hachure.ts


type alias Edge =
    { ymin : Float
    , ymax : Float
    , x : Float
    , islope : Float
    }


hachureAngle =
    -45


hachureLines : Float -> List Point -> List (List Point)
hachureLines thickness vertices =
    let
        edges =
            rotatePoints (degrees hachureAngle) vertices
                |> Helpers.List.segments True
                |> List.filterMap segmentToEdge
                |> List.sortWith sortEdges

        ymin =
            List.head edges
                |> Maybe.map .ymin
                |> Maybe.withDefault 0

        ymax =
            List.reverse edges
                |> List.head
                |> Maybe.map .ymax
                |> Maybe.withDefault 0

        yValues =
            List.range (round ymin) (round ymax)
                |> List.filter (\i -> modBy (ceiling thickness) i == 0)
                |> List.map toFloat
    in
    yValues
        |> List.concatMap (horizontalLine edges)
        |> List.map (rotatePoints (degrees -hachureAngle))


horizontalLine : List Edge -> Float -> List (List Point)
horizontalLine edges y =
    List.map (\e -> { e | x = e.x + (y - e.ymin) * e.islope }) edges
        |> List.filter (\e -> e.ymin <= y && e.ymax > y)
        |> List.sortBy .x
        |> (\l ->
                case l of
                    [] ->
                        []

                    [ a, b ] ->
                        [ [ ( a.x, y ), ( b.x, y ) ] ]

                    list ->
                        pairs list
                            |> List.map (\( a, b ) -> [ ( a.x, y ), ( b.x, y ) ])
           )


segmentToEdge : ( Point, Point ) -> Maybe Edge
segmentToEdge ( ( x1, y1 ), ( x2, y2 ) ) =
    if y1 == y2 then
        Nothing

    else
        { ymin = min y1 y2
        , ymax = max y1 y2
        , x =
            if y1 <= y2 then
                x1

            else
                x2
        , islope =
            (x2 - x1) / (y2 - y1)
        }
            |> Just


sortEdges : Edge -> Edge -> Basics.Order
sortEdges a b =
    if a.ymin /= b.ymin then
        compare a.ymin b.ymin

    else if a.x /= b.x then
        compare a.x b.x

    else
        compare a.ymax b.ymax


rotatePoints : Float -> List Point -> List Point
rotatePoints radians ps =
    List.map
        (\( x, y ) ->
            ( x * cos radians - y * sin radians
            , x * sin radians + y * cos radians
            )
        )
        ps


pairs : List a -> List ( a, a )
pairs list =
    List.map2 Tuple.pair list (Helpers.List.rotate list)
        |> List.indexedMap Tuple.pair
        |> List.filter (\( i, _ ) -> modBy 2 i == 0)
        |> List.unzip
        |> Tuple.second
