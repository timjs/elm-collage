module Path exposing (main)

import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Color exposing (..)
import Html exposing (Html)


zigzag : Collage msg
zigzag =
    let
        points =
            [ ( 0, 50 )
            , ( 50, 0 )
            , ( 50, 50 )
            , ( 100, 0 )
            , ( 100, 50 )
            , ( 150, 0 )
            ]
    in
    vertical
        [ path points
                |> traced (solid thin (uniform red))
        , spacer 0 20
        , curve points
                |> traced (solid thin (uniform red))
        , spacer 0 20
        ]


main : Html msg
main =
    zigzag |> svg
