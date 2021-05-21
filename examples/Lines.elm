module Lines exposing (lines, main)

import Example
import Collage exposing (..)
import Collage.Layout exposing (..)
import Color exposing (..)
import Html exposing (Html)


hline : Float -> Collage msg
hline t =
  line 100
    |> traced (solid t (uniform black))


gap : Collage msg
gap = spacer 50 50


lines : Collage msg
lines =
  vertical <|
    List.intersperse gap <|
      List.map hline [ ultrathin, verythin, thin, semithick, thick, verythick, ultrathick ]


collage =
  horizontal
    [ gap
    , vertical
        [ gap
        , lines
        , gap
        ]
    ]

main : Platform.Program () (Example.Model () (Collage ())) (Example.Msg ())
main =
    Example.example
        { init = collage
        , update = (\_ _ -> collage)
        , render = (\_ -> collage)
        , view = identity
        }

