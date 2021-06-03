module Diamond exposing (main)

import Example
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Color
import Html exposing (Html)


collage =
    let
        thicknesses =
            [ ultrathin, verythin, thin, semithick, thick, verythick, ultrathick ]
    in
    vertical
        (thicknesses
            |> List.concatMap (\t -> [spacer 10 10, diamond t])
        )

diamond : Float -> Collage msg
diamond thickness =
    let
        unit = 100

        w =
            unit

        l =
            unit / 2

        points =
            [ ( 0, l )
            , ( -l, 0 )
            , ( 0, -l )
            , ( w, -l )
            , ( w + l, 0 )
            , ( w, l )
            ]
    in
    polygon points
        |> styled
            ( uniform Color.lightPurple
            , solid thickness (uniform Color.blue)
            )
        |> center

main : Platform.Program () (Example.Model () (Collage ())) (Example.Msg ())
main =
    Example.example
        { init = collage
        , update = (\_ _ -> collage)
        , render = (\_ -> collage)
        , view = identity
        }
