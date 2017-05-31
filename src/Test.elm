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
    beside (part1 model) (part2 model)


part1 model =
    circle 50
        |> styled
            (uniform <|
                if model.active then
                    red
                else
                    blue
            )
            (solid 5 (uniform black))
        |> onClick Switch


part2 model =
    rectangle 100 50
        |> styled
            (uniform <|
                if model.active then
                    orange
                else
                    purple
            )
            (solid 5 (uniform black))


view : Model -> Html Msg
view model =
    drawing model
        |> svg 500 500



-- Main ------------------------------------------------------------------------


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = init, view = view, update = update }
