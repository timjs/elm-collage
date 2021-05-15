module Collage.Sketchy exposing (sketchy)

import Collage exposing (Collage, Point)
import Collage.Core as Core
import Random
import Random.Extra


type alias Config =
    { roughness : Float
    }


segments : Bool -> List Point -> List (Point, Point)
segments closed ps =
    List.map2
        Tuple.pair
        ps
        (List.drop 1 ps ++ List.take (List.length ps - 1) ps)
        |> (if closed then
                List.take (List.length ps)
            else
                List.take ((List.length ps) - 1))

sketchLines : Config -> List Point -> Random.Generator (List (List Point))
sketchLines config ps =
    segments True ps
        |> List.concatMap (\(a, b) -> [ sketchPoints config [ a, b ], sketchPoints config [ a, b ] ])
        |> Random.Extra.combine


sketchPoints : Config -> List Point -> Random.Generator (List Point)
sketchPoints config ps =
    let
        curvedPs =
            List.map2
                (\( x1, y1 ) ( x2, y2 ) -> [ ( x1, y1 ), ( x1 + (x2 - x1) / 2, y1 + (y2 - y1) / 2 ) ])
                ps
                (List.drop 1 ps ++ List.take (List.length ps - 1) ps)
                |> List.concat
                |> List.take ((List.length ps * 2) - 1)

        lineLength =
            List.map2
                (\( x1, y1 ) ( x2, y2 ) -> ( x2 - x1 ) ^ 2 + (y2 - y1) ^ 2 |> sqrt)
                ps
                (List.drop 1 ps ++ List.take (List.length ps - 1) ps)
                |> List.take (List.length ps)
                |> List.sum

        roughness =
            --FROM https://github.com/rough-stuff/rough/blob/master/src/renderer.ts#L274
            (if lineLength > 200 then
                1
            else if lineLength > 500 then
                0.4
            else
                (-0.0016668) * lineLength + 1.233334) * config.roughness

        randomOffset =
            Random.pair (Random.float (0 - roughness) roughness) (Random.float (0 - roughness) roughness)

    in
    Random.list (List.length ps) randomOffset
        |> Random.map
            (\shifts ->
                List.map2
                    (\( x, y ) ( shiftX, shiftY ) ->
                        ( x + shiftX, y + shiftY )
                    )
                    ps
                    shifts
            )


sketchy : Collage msg -> Random.Generator (Collage msg)
sketchy collage =
    case collage.basic of
        Core.Path style path ->
            case path of
                Core.Polyline ps ->
                    Random.map2
                        (\points1 points2 ->
                            Collage.group
                                [ { collage | basic = Core.Path style (Core.Curve points1) }
                                , { collage | basic = Core.Path style (Core.Curve points2) }
                                ]
                        )
                        (sketchPoints { roughness = 2 } ps)
                        (sketchPoints { roughness = 2 } ps)

                Core.Curve ps ->
                    Random.constant collage

        Core.Shape ( fill, line ) path ->
            case path of
                Core.Polygon ps ->
                    sketchLines { roughness = 3 } ps
                        |> Random.map2
                            (\points lines ->
                                Collage.group <|
                                    (List.map (\segment -> { collage | basic = Core.Path line (Core.Curve segment) }) lines)
                                    ++ [ { collage | basic = Core.Shape ( fill, Collage.invisible ) (Core.Polygon points) } ]
                            )
                            (sketchPoints { roughness = 2 } ps)

                Core.Rectangle w h r ->
                    let
                        ps =
                            [ ( -w / 2, -h / 2 )
                            , ( w / 2, -h / 2 )
                            , ( w / 2, h / 2 )
                            , ( -w / 2, h / 2 )
                            ]

                        sketchedLines =
                            sketchLines { roughness = 3 } ps
                                |> Random.map2
                                    (\points lines ->
                                        Collage.group <|
                                            (List.map (\segment -> { collage | basic = Core.Path line (Core.Curve segment) }) lines)
                                            ++ [ { collage | basic = Core.Shape ( fill, Collage.invisible ) (Core.Polygon points) } ]
                                    )
                                    (sketchPoints { roughness = 2 } ps)
                    in
                    sketchedLines

                _ ->
                    Random.constant collage

        Core.Group collages ->
            Random.Extra.combine (List.map sketchy collages)
                |> Random.map (\group -> { collage | basic = Core.Group group })

        Core.Subcollage fore back ->
            Random.Extra.combine (List.map sketchy [ fore, back ])
                |> Random.map (\group -> { collage | basic = Core.Group group })

        _ ->
            Random.constant collage
