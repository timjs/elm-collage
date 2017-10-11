module Axis exposing (main)

import Collage exposing (..)
import Collage.Render exposing (..)
import Collage.Text exposing (Text, fromString)
import Color exposing (..)
import Html exposing (Html)


main : Html msg
main =
    let
        up =
            triangle 10
                |> filled (uniform black)

        right =
            triangle 10
                |> filled (uniform black)
                |> rotate (degrees -90)

        xAxis =
            segment ( -100, 0 ) ( 100, 0 )
                |> traced (solid thin (uniform black))

        yAxis =
            segment ( 0, -100 ) ( 0, 100 )
                |> traced (solid thin (uniform black))

        ex =
            fromString "x"
                |> rendered

        wy =
            fromString "y"
                |> rendered

        box =
            square 220
                |> outlined (dot thin (uniform black))
    in
    group
        [ xAxis
        , right |> shift ( 100, 0 )
        , ex |> shift ( 100, -10 )
        , yAxis
        , up |> shift ( 0, 100 )
        , wy |> shift ( -10, 100 )
        , box
        , ellipse 20 40 |> filled (uniform red) |> shift ( 50, 50 )
        ]
        |> svg
