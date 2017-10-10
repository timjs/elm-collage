module Simple exposing (main)

import Collage exposing (circle, filled, rectangle, uniform)
import Collage.Layout exposing (at, topLeft)
import Collage.Render exposing (svg)
import Color
import Html exposing (Html)


main : Html msg
main =
    let
        circ =
            circle 50
                |> filled (uniform Color.red)

        rect =
            rectangle 200 100
                |> filled (uniform Color.blue)
    in
    rect
        |> at topLeft circ
        |> svg
