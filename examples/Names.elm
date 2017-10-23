module Names exposing (main)

import Collage exposing (..)
import Collage.Core as Core
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Color exposing (..)
import Html exposing (Html, text)
import Maybe.Extra exposing ((?))


-- Elements --------------------------------------------------------------------


rect1 : Collage msg
rect1 =
    square 7
        |> filled (uniform brown)
        |> scale 10
        |> name "rect1"


rect2 : Collage msg
rect2 =
    rectangle 5 7
        |> filled (uniform green)
        |> scale 10
        |> rotate (degrees 30)
        |> name "rect2"


rect3 : Collage msg
rect3 =
    rectangle 100 50
        |> filled (uniform purple)
        |> name "rect3"


circ : Collage msg
circ =
    circle 50
        |> filled (uniform blue)
        |> name "circ"


mark : Collage msg
mark =
    circle 3
        |> filled (uniform yellow)
        |> name "mark"


collage : Collage msg
collage =
    let
        inner =
            mark
                |> shift (locate "circ" topRight circ ? ( 0, 0 ))

        background =
            --NOTE: rect2 and rect3 have the same name, rect2 should be found first.
            horizontal [ rect1, rect2, rect3 ]
    in
    background



-- Main ------------------------------------------------------------------------


main : Html msg
main =
    Html.div []
        [ svg collage
        , Html.p []
            [ { names =
                    names collage
              , rectTopRight =
                    locate "rect" topRight collage
              , rect =
                    Core.search (.name >> Maybe.map ((==) "rect") >> Maybe.withDefault False) collage
              , levels =
                    List.map .name <| Core.levels collage
              }
                |> toString
                |> text
            ]
        ]
