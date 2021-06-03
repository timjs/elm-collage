module Collage.Sketchy.Fill exposing (hachureLines)


import Collage.Core as Core exposing (FillStyle(..))
import Collage exposing (..)
import Color exposing (..)
import Collage.Sketchy.Helpers exposing (segments)


type alias Edge =
    { ymin: Float
    , ymax: Float
    , x: Float
    , islope: Float
    }


hachureLines : List Point -> List Core.Path
hachureLines vertices =
    let
        edges =
            segments True vertices
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
                |> List.filter (\i -> modBy 5 i == 0)
                |> List.map toFloat
    in
    yValues
        |> List.concatMap (horizontalLine edges)


horizontalLine : List Edge -> Float -> List Path
horizontalLine edges y =
    List.map (\e -> { e | x = e.x + (y - e.ymin) * e.islope }) edges
        |> List.filter (\e -> e.ymin <= y && e.ymax > y)
        |> List.sortBy .x
        |> (\l ->
            case l of
                [] ->
                    []

                [ a, b ] ->
                    [ segment (a.x, y) (b.x, y) ]

                _ ->
                    Debug.todo "complex polygon"
            )


segmentToEdge : (Point, Point) -> Maybe Edge
segmentToEdge ((x1, y1), (x2, y2)) =
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


