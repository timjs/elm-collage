module Main exposing (..)

import Html exposing (Html)
import Color exposing (..)
import Text exposing (fromString)
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Events exposing (onClick)
import Collage.Render exposing (svg)


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


txt : Collage Msg
txt =
    fromString "Hallo"
        |> centered


circ : Model -> Collage Msg
circ model =
    circle 50
        |> styled
            ( uniform <|
                if model.active then
                    lightRed
                else
                    lightBlue
            , border
            )
        |> onClick Switch



--|> showEnvelope
--|> showOrigin


rect : Collage Msg
rect =
    rectangle 100 100
        |> styled ( uniform lightOrange, border )
        |> translate ( 25, -25 )



--|> showEnvelope
--|> showOrigin


tria : Collage msg
tria =
    triangle 50
        |> styled ( uniform lightGreen, border )



--|> showEnvelope
--|> showOrigin


hline : Float -> Collage msg
hline t =
    line 100
        |> traced (solid t (uniform black))


lines1 : Collage msg
lines1 =
    vertical <|
        List.intersperse (spacer 50 50) <|
            List.map hline [ ultrathin, verythin, thin, semithick, thick, verythick, ultrathick ]


lines2 : Collage msg
lines2 =
    vertical <|
        List.intersperse (spacer 50 50) <|
            List.map hline [ 0.5, 1, 2, 4, 6, 8, 12, 16 ]


border : LineStyle
border =
    solid verythin <| uniform black


view : Model -> Html Msg
view model =
    horizontal [ lines1, lines2 ]
        |> svg 500 500



--vertical [ rect, stack [ txt, circ model ], tria ]
-- ==
--rect model
--    |> above (circ model)
--    |> above (tria model)
--|> showEnvelope
--|> showOrigin
-- Main ------------------------------------------------------------------------


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = init, view = view, update = update }
