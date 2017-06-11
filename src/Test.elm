module Main exposing (..)

import Collage exposing (..)
import Collage.Events exposing (onClick)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Color exposing (..)
import Html exposing (Html)
import Text exposing (fromString)


-- Model -----------------------------------------------------------------------


type alias Model =
    { active : Bool }


init : Model
init =
    { active = False }



-- Update ----------------------------------------------------------------------


type Msg
    = Switch


update : Msg -> Model -> Model
update msg model =
    case msg of
        Switch ->
            { model | active = not model.active }



-- View ------------------------------------------------------------------------
-- Styles --


border : LineStyle
border =
    solid verythin <| uniform black


debug : Collage msg -> Collage msg
debug collage =
    collage |> showOrigin



-- Text --


txt : Collage Msg
txt =
    text <| fromString "Hallo"



-- Shapes --


circ : Model -> Collage Msg
circ model =
    circle 50
        |> styled
            ( uniform <|
                if model.active then
                    lightPurple
                else
                    lightBlue
            , border
            )
        |> shift ( 20, 30 )
        |> onClick Switch


rect : Collage msg
rect =
    rectangle 100 100
        |> styled ( uniform lightOrange, border )


tria : Collage msg
tria =
    triangle 50
        |> styled ( uniform lightGreen, border )



-- Lines --


hline : Float -> Collage msg
hline t =
    line 100
        |> traced (solid t (uniform black))


lines : Collage msg
lines =
    vertical <|
        List.intersperse (spacer 50 50) <|
            List.map hline [ ultrathin, verythin, thin, semithick, thick, verythick, ultrathick ]


alignments : Collage msg
alignments =
    horizontal <|
        List.map (showOrigin << align top) [ rect, tria, rect, rect ]



-- Main ------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    alignments
        --horizontal [ debug <| vertical [ circ model, debug rect ], debug rect ]
        --horizontal [ lines1, lines2 ]
        --vertical [ rect, stack [ txt, circ model ], tria ]
        -- ==
        --rect model
        --    |> above (circ model)
        --    |> above (tria model)
        -- |> showEnvelope
        -- |> showOrigin
        |> debug
        |> svg


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = init, view = view, update = update }
