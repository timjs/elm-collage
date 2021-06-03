module Collage.Sketchy exposing (Config, defaultConfig, sketchy, nextSeed)

{-| Transform a collage so it looks rough and hand drawn.

@docs Config, defaultConfig, sketchy, nextSeed

-}

import Array
import Collage exposing (Collage, Point)
import Collage.Core as Core
import Collage.Sketchy.Fill as Fill
import Collage.Sketchy.Helpers exposing (rotateList, segments)


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
    { roughness = 2, bowing = 1, seed = 0 }


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
            let
                sketchPath ps =
                    sketchLines False config ps
                        |> List.map (\segment -> Collage.curve segment |> Collage.traced style)
            in
            case path of
                Core.Polyline ps ->
                    { collage | basic = Core.Group (sketchPath ps) }

                Core.Curve ps ->
                    { collage | basic = Core.Group (sketchPath ps) }

        Core.Shape ( fill, line ) path ->
            -- FIXME: Use hachures for fills or at least curve shape edges.
            let
                sketchPolygon ps =
                    sketchLines True config ps
                        |> List.map (\segment -> Collage.curve segment |> Collage.traced line)

                hachureThickness =
                    max 1 (line.thickness - 1)

                sketchFill ps =
                    Fill.hachureLines hachureThickness ps
                        |> List.indexedMap (\i ends -> sketchPoints { config | seed = config.seed + i, roughness = 1 } ends |> Collage.curve)
                        |> List.map (Collage.solid hachureThickness fill |> Collage.traced)

                sketchEllipse ps =
                    sketchPoints { config | bowing = 0 } (ps ++ rotateList ps)
                        |> Collage.curve
                        |> Collage.traced line
            in
            case path of
                Core.Polygon ps ->
                    { collage | basic = Core.Group <| sketchPolygon ps ++ sketchFill ps }

                Core.Rectangle w h r ->
                    let
                        ps =
                            [ ( -w / 2, -h / 2 )
                            , ( w / 2, -h / 2 )
                            , ( w / 2, h / 2 )
                            , ( -w / 2, h / 2 )
                            ]
                    in
                    { collage | basic = Core.Group <| sketchPolygon ps ++ sketchFill ps }

                Core.Circle r ->
                    let
                        ps =
                            ellipsePoints r r
                    in
                    { collage | basic = Core.Group <| [ sketchEllipse ps ] ++ sketchFill ps }

                Core.Ellipse rx ry ->
                    let
                        ps =
                            ellipsePoints rx ry
                    in
                    { collage | basic = Core.Group <| [ sketchEllipse ps ] ++ sketchFill ps }

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


sketchLines : Bool -> Config -> List Point -> List (List Point)
sketchLines closed config ps =
    segments closed ps
        |> List.concatMap (\( a, b ) -> [ sketchPoints config [ a, b ], sketchPoints (nextSeed config) [ a, b ] ])


sketchPoints : Config -> List Point -> List Point
sketchPoints config ps =
    let
        bowedPs =
            if config.bowing == 0 then
                ps

            else
                segments True ps
                    |> List.map
                        (\( ( x1, y1 ), ( x2, y2 ) ) ->
                            [ ( x1, y1 )
                            , ( x1 + (x2 - x1) / 2, y1 + (y2 - y1) / 2 )
                            ]
                        )
                    |> List.concat
                    |> List.take ((List.length ps * 2) - 1)

        lineLength =
            segments True ps
                |> List.map
                    (\( ( x1, y1 ), ( x2, y2 ) ) -> (x2 - x1) ^ 2 + (y2 - y1) ^ 2 |> sqrt)
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
        |> Array.get (modBy 100 i)
        |> Maybe.withDefault 0.5
