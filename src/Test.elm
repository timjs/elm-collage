module Main exposing (..)

import Html exposing (..)
import Color exposing (..)
import Collage exposing (..)
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
    circle 150
        |> filledAndStroked
            (uniform
                (if model.active then
                    red
                 else
                    blue
                )
            )
            (solid 5 (uniform black))
        |> move ( 200, 200 )
        |> onClick Switch


view : Model -> Html Msg
view model =
    drawing model
        |> svg 500 500



-- Main ------------------------------------------------------------------------


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = init, view = view, update = update }
