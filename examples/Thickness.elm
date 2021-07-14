module Thickness exposing (main)

import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Color
import Example
import Html exposing (Html)


collage =
    let
        thicknesses =
            [ ultrathin, verythin, thin, semithick, thick, verythick, ultrathick ]
    in
    horizontal
        [ vertical
            (thicknesses
                |> List.concatMap (\t -> [ spacer 10 10, diamond t ])
            )
        , vertical
            (thicknesses
                |> List.concatMap (\t -> [ spacer 10 15, zigzag t ])
            )
        ]


diamond : Float -> Collage msg
diamond thickness =
    let
        unit =
            100

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


zigzag : Float -> Collage msg
zigzag thickness =
    let
        top =
            [ ( 0, 50 )
            , ( 50, 0 )
            , ( 50, 50 )
            , ( 100, 0 )
            , ( 100, 50 )
            , ( 150, 0 )
            ]

        bottom =
            top |> List.map (\( x, y ) -> ( x - 20, y + 50 )) |> List.reverse
    in
    polygon (top ++ bottom)
        |> styled ( uniform Color.red, solid thickness (uniform Color.black) )


main : Platform.Program () (Example.Model () (Collage ())) (Example.Msg ())
main =
    Example.example
        { init = collage
        , update = \_ _ -> collage
        , render = \_ -> collage
        , view = identity
        }
