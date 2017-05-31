module Main exposing (..)

import Html exposing (Html)
import Color exposing (..)
import Text exposing (..)
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
            (uniform <|
                if model.active then
                    lightRed
                else
                    lightBlue
            )
            border
        |> onClick Switch
        --|> showEnvelope
        --|> showOrigin


rect : Collage Msg
rect =
    rectangle 100 100
        |> styled (uniform lightOrange) border
        |> translate ( 25, -25 )
        --|> showEnvelope
        --|> showOrigin


tria : Collage msg
tria =
    triangle 50
        |> styled (uniform lightGreen) border
        --|> showEnvelope
        --|> showOrigin


border : LineStyle
border =
    solid 5 <| uniform black


view : Model -> Html Msg
view model =
    vertical [ rect, stack [txt, circ model], tria ]
    -- ==
    --rect model
    --    |> above (circ model)
    --    |> above (tria model)
        --|> showEnvelope
        --|> showOrigin
        |> svg 500 500



-- Main ------------------------------------------------------------------------


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = init, view = view, update = update }
