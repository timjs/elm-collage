module Main exposing (..)

import Html exposing (..)
import Color exposing (..)
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Events exposing (onClick)
import Collage.Svg exposing (svg)


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


drawing : Model -> Form Msg
drawing model =
    -- horizontal [ rect model, circ model ] ==
    before (rect model) (circ model)
        |> showEnvelope
        |> showOrigin


circ : Model -> Form Msg
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
        |> showEnvelope
        |> showOrigin


rect : Model -> Form Msg
rect model =
    rectangle 100 100
        |> styled (uniform lightOrange) border
        |> translate ( 25, -25 )
        |> showEnvelope
        |> showOrigin


tria : Model -> Form msg
tria model =
    triangle 50
        |> styled (uniform lightGreen) border
        |> showEnvelope
        |> showOrigin


border : LineStyle
border =
    solid 5 <| uniform black


view : Model -> Html Msg
view model =
    rect model
        |> above (circ model)
        |> above (tria model)
        |> showEnvelope
        |> showOrigin
        |> svg 500 500



-- Main ------------------------------------------------------------------------


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = init, view = view, update = update }
