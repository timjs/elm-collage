module Composition exposing (main)

import Browser
import Collage exposing (..)
import Collage.Events exposing (onClick)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Collage.Text exposing (fromString)
import Collage.Sketchy exposing (sketchy)
import Color exposing (..)
import Html exposing (Html)
import Random


-- Model -----------------------------------------------------------------------


type alias Model =
  { active : Bool, collage : Collage Msg }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            Model False txt
    in
    ( model, sketchy (render model) |> Random.generate GeneratedSketchy )



-- Update ----------------------------------------------------------------------


type Msg
    = Switch
    | GeneratedSketchy (Collage Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Switch ->
        ( { model | active = not model.active }, Cmd.none )
    GeneratedSketchy collage ->
        ( { model | collage = collage }, Cmd.none )



-- View ------------------------------------------------------------------------
-- Styles --


border : LineStyle
border = solid verythin <| uniform black



-- Text --


txt : Collage Msg
txt =
  fromString "Hello collage!"
    |> rendered



-- Shapes --


elps : Model -> Collage Msg
elps model =
  ellipse 100 50
    |> styled
        ( uniform <|
            if model.active then
              lightPurple
            else
              lightBlue
        , border
        )
    |> rotate (degrees -30)
    |> onClick Switch


rect : Collage msg
rect =
  roundedRectangle 200 250 20
    |> styled ( uniform lightOrange, border )


tria : Collage msg
tria =
  triangle 100
    |> styled ( uniform lightGreen, border )


penta : Collage msg
penta =
  ngon 5 100
    |> styled ( uniform lightCharcoal, border )



-- Alignments --


alignments : Collage msg
alignments =
  horizontal <|
    List.map (showOrigin << align top) [ rect, tria, rect, rect ]



-- Main ------------------------------------------------------------------------

render model =
  vertical
    [ horizontal
        [ rect
        , vertical
            [ tria
            , tria |> rotate pi
            ]
            |> center
        , debug penta
        ]
    , stack [ showEnvelope txt, elps model ]
    ]

view : Model -> Html Msg
view model =
    model.collage |> svg


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }

