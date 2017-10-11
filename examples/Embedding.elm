module Embedding exposing (main)

import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (..)
import Color exposing (..)
import Html exposing (Html, text)


main : Html msg
main =
    stack
        [ Html.div []
            [ Html.button []
                [ text "Hello Html!" ]
            ]
            |> html ( 100, 100 )
        , rectangle 100 100
            |> filled (uniform Color.lightGreen)
        ]
        |> rotate (degrees 30)
        |> svg
