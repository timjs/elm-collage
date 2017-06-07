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
        |> translate ( 20, 30 )
        |> onClick Switch


rect : Collage Msg
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



-- Main ------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    rect
        |> translate ( 50, 50 )
        |> showOrigin
        -- circ model
        --vertical [ debug (circ model), debug rect ]
        --horizontal [ lines1, lines2 ]
        --vertical [ rect, stack [ txt, circ model ], tria ]
        -- ==
        --rect model
        --    |> above (circ model)
        --    |> above (tria model)
        -- |> showEnvelope
        -- |> showOrigin
        |> svg


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = init, view = view, update = update }
