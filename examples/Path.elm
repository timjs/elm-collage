module Path exposing (main)

import Collage exposing (..)
import Collage.Render exposing (svg)
import Color exposing (..)
import Html exposing (Html)


zigzag : Collage msg
zigzag =
    path [ ( 0, 50 ), ( 50, 0 ), ( 50, 50 ) ]
        |> traced (solid thin (uniform red))


main : Html msg
main =
    zigzag |> svg
