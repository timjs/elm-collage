module Lines exposing (lines, main)

import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Color exposing (..)
import Html exposing (Html)


hline : Float -> Collage msg
hline t =
    line 100
        |> traced (solid t (uniform black))


gap : Collage msg
gap =
    spacer 50 50


lines : Collage msg
lines =
    vertical <|
        List.intersperse gap <|
            List.map hline [ ultrathin, verythin, thin, semithick, thick, verythick, ultrathick ]


main : Html msg
main =
    horizontal
        [ gap
        , vertical
            [ gap
            , lines
            , gap
            ]
        ]
        |> svg
