module Collage.Sketchy exposing (Config, defaultConfig, sketchy)

import Collage exposing (Collage, Point)
import Collage.Core as Core
import Random
import Random.Extra


type alias Config =
    { roughness : Float
    , bowing : Float
    }


defaultConfig : Config
defaultConfig =
    { roughness = 2, bowing = 1 }


sketchy : Config -> Collage msg -> Random.Generator (Collage msg)
sketchy config collage =
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
                        (sketchPoints config ps)
                        (sketchPoints config ps)

                Core.Curve ps ->
                    Random.constant collage

        Core.Shape ( fill, line ) path ->
            case path of
                Core.Polygon ps ->
                    sketchLines config ps
                        |> Random.map2
                            (\points lines ->
                                Collage.group <|
                                    List.map (\segment -> { collage | basic = Core.Path line (Core.Curve segment) }) lines
                                        ++ [ { collage | basic = Core.Shape ( fill, Collage.invisible ) (Core.Polygon points) } ]
                            )
                            (sketchPoints config ps)

                Core.Rectangle w h r ->
                    let
                        ps =
                            [ ( -w / 2, -h / 2 )
                            , ( w / 2, -h / 2 )
                            , ( w / 2, h / 2 )
                            , ( -w / 2, h / 2 )
                            ]
                    in
                    sketchLines config ps
                        |> Random.map2
                            (\points lines ->
                                Collage.group <|
                                    List.map (\segment -> { collage | basic = Core.Path line (Core.Curve segment) }) lines
                                        ++ [ { collage | basic = Core.Shape ( fill, Collage.invisible ) (Core.Polygon points) } ]
                            )
                            (sketchPoints config ps)

                Core.Circle r ->
                    let
                        ps =
                            ellipsePoints r r
                    in
                    Random.map2
                        (\points1 points2 ->
                            Collage.group <|
                                [ { collage | basic = Core.Path line (Core.Curve (points1 ++ rotate points2)) }
                                ]
                                    ++ [ { collage | basic = Core.Shape ( fill, Collage.invisible ) (Core.Circle r) } ]
                        )
                        (sketchPoints { config | bowing = 0 } ps)
                        (sketchPoints { config | bowing = 0 } ps)

                Core.Ellipse rx ry ->
                    let
                        ps =
                            ellipsePoints rx ry
                    in
                    Random.map2
                        (\points1 points2 ->
                            Collage.group <|
                                [ { collage | basic = Core.Path line (Core.Curve (points1 ++ rotate points2)) }
                                ]
                                    ++ [ { collage | basic = Core.Shape ( fill, Collage.invisible ) (Core.Ellipse rx ry) } ]
                        )
                        (sketchPoints { config | bowing = 0 } ps)
                        (sketchPoints { config | bowing = 0 } ps)

                _ ->
                    Random.constant collage

        Core.Group collages ->
            Random.Extra.combine (List.map (sketchy config) collages)
                |> Random.map (\group -> { collage | basic = Core.Group group })

        Core.Subcollage fore back ->
            Random.map2
                (\sketchedFore sketchedBack -> { collage | basic = Core.Subcollage sketchedFore sketchedBack })
                (sketchy config fore)
                (sketchy config back)

        _ ->
            Random.constant collage



-- INTERNAL


segments : Bool -> List Point -> List ( Point, Point )
segments closed ps =
    List.map2
        Tuple.pair
        ps
        (List.drop 1 ps ++ List.take (List.length ps - 1) ps)
        |> (if closed then
                List.take (List.length ps)

            else
                List.take (List.length ps - 1)
           )


rotate : List a -> List a
rotate list =
    case list of
        head :: tail ->
            tail ++ [ head ]

        _ ->
            list


sketchLines : Config -> List Point -> Random.Generator (List (List Point))
sketchLines config ps =
    segments True ps
        |> List.concatMap (\( a, b ) -> [ sketchPoints config [ a, b ], sketchPoints config [ a, b ] ])
        |> Random.Extra.combine


sketchPoints : Config -> List Point -> Random.Generator (List Point)
sketchPoints config ps =
    let
        bowedPs =
            if config.bowing == 0 then
                ps

            else
                List.map2
                    (\( x1, y1 ) ( x2, y2 ) -> [ ( x1, y1 ), ( x1 + (x2 - x1) / 2, y1 + (y2 - y1) / 2 ) ])
                    ps
                    (List.drop 1 ps ++ List.take (List.length ps - 1) ps)
                    |> List.concat
                    |> List.take ((List.length ps * 2) - 1)

        lineLength =
            List.map2
                (\( x1, y1 ) ( x2, y2 ) -> (x2 - x1) ^ 2 + (y2 - y1) ^ 2 |> sqrt)
                ps
                (List.drop 1 ps ++ List.take (List.length ps - 1) ps)
                |> List.take (List.length ps)
                |> List.sum

        roughness =
            (if lineLength > 200 then
                1

             else if lineLength > 500 then
                0.4

             else
                lineLength / 100
            )
                * config.roughness

        randomOffset =
            Random.pair (Random.float (0 - roughness) roughness) (Random.float (0 - roughness) roughness)
    in
    Random.list (List.length bowedPs) randomOffset
        |> Random.map
            (\shifts ->
                List.map2
                    (\( x, y ) ( shiftX, shiftY ) ->
                        ( x + shiftX, y + shiftY )
                    )
                    bowedPs
                    shifts
            )


ellipsePoints : Float -> Float -> List Point
ellipsePoints rx ry =
    let
        m r =
            r ^ 2 / 2 |> sqrt
    in
    [ ( 0, ry )
    , ( -(m rx), m ry )
    , ( -rx, 0 )
    , ( -(m rx), -(m ry) )
    , ( 0, -ry )
    , ( m rx, -(m ry) )
    , ( rx, 0 )
    , ( m rx, m ry )
    , ( 0, ry )
    ]
