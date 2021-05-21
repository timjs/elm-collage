module Simple exposing (main)

import Example
import Collage exposing (Collage, circle, filled, rectangle, uniform)
import Collage.Layout exposing (at, topLeft)
import Color
import Html exposing (Html)


collage =
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


main : Platform.Program () (Example.Model () (Collage ())) (Example.Msg ())
main =
    Example.example
        { init = collage
        , update = (\_ _ -> collage)
        , render = (\_ -> collage)
        , view = identity
        }
