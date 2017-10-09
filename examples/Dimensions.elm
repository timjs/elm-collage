module Dimensions exposing (diamond, main)

import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (..)
import Collage.Text as Text exposing (Shape(..), Text, fromString)
import Color exposing (Color)
import Html exposing (Html)


gap : Collage msg
gap =
    spacer 50 50


diamond : String -> Collage msg
diamond label =
    let
        text =
            fromString label
                |> Text.shape Italic
                |> rendered

        w =
            width text

        points =
            [ ( 0, 20 )
            , ( -20, 0 )
            , ( 0, -20 )
            , ( w, -20 )
            , ( w + 20, 0 )
            , ( w, 20 )
            ]

        shape =
            polygon points
                |> styled
                    ( uniform (Color.rgb 255 202 255)
                    , solid thin (uniform Color.black)
                    )
                |> center
    in
    stack
        [ text
        , shape
        ]


main : Html msg
main =
    vertical
        [ diamond "a very long piece of text"
        , gap
        , diamond "short text"
        ]
        |> svg
