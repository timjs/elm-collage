module Collage.Sketchy exposing
    ( Config, defaultConfig, sketchy
    , nextSeed
    )

{-| Transform a collage so it looks rough and hand drawn.

@docs Config, defaultConfig, sketchy, nextSeed

-}

import Array
import Collage exposing (Collage, Point)
import Collage.Core as Core


{-| Configure how rough results should look.

  - `roughness` controls how far points will be shifted from original locations.
  - `bowing` controls depth of curvature between two points on a line. Currently only responds to 0 or 1 values.
  - `seed` controls random number generator. Reuse the same seed to reproduce an identical sketched collage.

-}
type alias Config =
    { roughness : Float
    , bowing : Float
    , seed : Int
    }


{-| Default configuration values.

    { roughness = 2, bowing = 1, seed = 0 }

-}
defaultConfig : Config
defaultConfig =
    { roughness = 3, bowing = 1, seed = 0 }


{-| Helper for incrementing the seed value to generate a new randomized Sketchy collage.

    sketchy (nextSeed config) collage

-}
nextSeed : Config -> Config
nextSeed config =
    { config | seed = config.seed + 1 }


{-| Generate a sketched version of a collage.

    sketchy defaultConfig collage
        |> Collage.Render.svg

-}
sketchy : Config -> Collage msg -> Collage msg
sketchy config collage =
    case collage.basic of
        Core.Path style path ->
            case path of
                Core.Polyline ps ->
                    (\points1 points2 ->
                        Collage.group
                            [ { collage | basic = Core.Path style (Core.Curve points1) }
                            , { collage | basic = Core.Path style (Core.Curve points2) }
                            ]
                    )
                        (sketchPoints config ps)
                        (sketchPoints (nextSeed config) ps)

                Core.Curve ps ->
                    (\points1 points2 ->
                        Collage.group
                            [ { collage | basic = Core.Path style (Core.Curve points1) }
                            , { collage | basic = Core.Path style (Core.Curve points2) }
                            ]
                    )
                        (sketchPoints config ps)
                        (sketchPoints (nextSeed config) ps)

        Core.Shape ( fill, line ) path ->
            -- FIXME: Use hachures for fills or at least curve shape edges.
            case path of
                Core.Polygon ps ->
                    (\points lines ->
                        Collage.group <|
                            List.map (\segment -> { collage | basic = Core.Path line (Core.Curve segment) }) lines
                                ++ [ { collage | basic = Core.Shape ( fill, Collage.invisible ) (Core.Polygon points) } ]
                    )
                        (sketchPoints config ps)
                        (sketchLines (nextSeed config) ps)

                Core.Rectangle w h r ->
                    let
                        ps =
                            [ ( -w / 2, -h / 2 )
                            , ( w / 2, -h / 2 )
                            , ( w / 2, h / 2 )
                            , ( -w / 2, h / 2 )
                            ]
                    in
                    (\points lines ->
                        Collage.group <|
                            List.map (\segment -> { collage | basic = Core.Path line (Core.Curve segment) }) lines
                                ++ [ { collage | basic = Core.Shape ( fill, Collage.invisible ) (Core.Polygon points) } ]
                    )
                        (sketchPoints config ps)
                        (sketchLines config ps)

                Core.Circle r ->
                    let
                        ps =
                            ellipsePoints r r
                    in
                    (\points1 points2 ->
                        Collage.group <|
                            [ { collage | basic = Core.Path line (Core.Curve (points1 ++ rotate points2)) }
                            ]
                                ++ [ { collage | basic = Core.Shape ( fill, Collage.invisible ) (Core.Circle r) } ]
                    )
                        (sketchPoints { config | bowing = 0 } ps)
                        (sketchPoints (nextSeed { config | bowing = 0 }) ps)

                Core.Ellipse rx ry ->
                    let
                        ps =
                            ellipsePoints rx ry
                    in
                    (\points1 points2 ->
                        Collage.group <|
                            [ { collage | basic = Core.Path line (Core.Curve (points1 ++ rotate points2)) }
                            ]
                                ++ [ { collage | basic = Core.Shape ( fill, Collage.invisible ) (Core.Ellipse rx ry) } ]
                    )
                        (sketchPoints { config | bowing = 0 } ps)
                        (sketchPoints (nextSeed { config | bowing = 0 }) ps)

                _ ->
                    collage

        Core.Group collages ->
            List.map (sketchy (nextSeed config)) collages
                |> (\group -> { collage | basic = Core.Group group })

        Core.Subcollage fore back ->
            (\sketchedFore sketchedBack -> { collage | basic = Core.Subcollage sketchedFore sketchedBack })
                (sketchy (nextSeed config) fore)
                (sketchy (nextSeed config) back)

        _ ->
            collage



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


sketchLines : Config -> List Point -> List (List Point)
sketchLines config ps =
    segments True ps
        |> List.concatMap (\( a, b ) -> [ sketchPoints config [ a, b ], sketchPoints (nextSeed config) [ a, b ] ])


sketchPoints : Config -> List Point -> List Point
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

        randomOffset i =
            ( random (config.seed + i) * roughness, random (config.seed + i + 1) * roughness )
    in
    List.indexedMap
        (\i ( x, y ) ->
            let
                ( shiftX, shiftY ) =
                    randomOffset (i * 2)
            in
            ( x + shiftX, y + shiftY )
        )
        bowedPs


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


{-| Faster and easier way to shift points randomly.

Generated manually:

    $ ruby -e "puts 100.times.map{|i| Random.new.rand(-1.0..1.0).round(2) }.to_s"

-}
random : Int -> Float
random i =
    [ -0.99, -0.33, -0.84, 0.24, 0.45, 0.25, -0.63, -0.36, -0.4, -0.99, 0.21, -0.14, -0.96, -0.28, -0.17, 0.58, -0.65, 0.36, 0.38, -0.44, -0.33, 0.36, -0.72, -0.76, -0.92, -0.89, -0.82, -0.53, 0.25, 0.2, -0.9, -0.83, 0.22, 0.27, 0.05, -0.38, 0.68, -0.25, 0.8, 0.47, 0.62, 0.39, 0.74, -0.09, 0.23, -0.97, 0.21, 0.88, -0.32, -0.96, 0.01, -0.25, -0.99, -0.37, -0.73, -0.42, -0.54, 0.01, 0.95, -0.11, -0.59, -0.65, -0.28, 0.14, -0.22, -0.98, -0.9, 0.19, 0.35, 0.06, 0.53, 0.89, -0.01, 0.98, -0.35, 0.91, 0.49, 0.18, -0.99, 0.54, 0.45, -0.11, -0.91, -0.75, -0.61, -0.21, 0.9, 0.97, 0.68, 0.51, -0.18, 0.66, -0.05, 0.11, 0.98, 0.87, -0.88, 0.2, -0.82, -0.01 ]
        |> Array.fromList
        |> Array.get (modBy i 100)
        |> Maybe.withDefault 0.5
