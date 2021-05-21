module Embedding exposing (main)

import Example
import Collage exposing (..)
import Collage.Layout exposing (..)
import Color
import Html exposing (Html, text)


collage =
  stack
    [ Html.div []
        [ Html.button []
            [ text "Hello Html!" ]
        ]
        |> html ( 100, 100 ) []
    , rectangle 100 100
        |> filled (uniform Color.lightGreen)
    ]
    |> rotate (degrees 30)

main : Platform.Program () (Example.Model () (Collage ())) (Example.Msg ())
main =
    Example.example
        { init = collage
        , update = (\_ _ -> collage)
        , render = (\_ -> collage)
        , view = identity
        }
