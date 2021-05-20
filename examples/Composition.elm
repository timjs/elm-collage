module Composition exposing (main)

import Browser
import Collage exposing (..)
import Collage.Events exposing (onClick)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Collage.Text exposing (fromString)
import Collage.Sketchy as Sketchy exposing (sketchy)
import Color exposing (..)
import Html exposing (Html)
import Html.Events
import Random


-- Model -----------------------------------------------------------------------


type alias Model =
  { active : Bool, sketchy: Bool, collage : Collage Msg }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            Model False False (render False)
    in
    ( model, Cmd.none )



-- Update ----------------------------------------------------------------------


type Msg
    = Switch
    | ClickedNormal
    | ClickedSketchy
    | GeneratedSketchy (Collage Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        renderModel m =
            ( { m | collage = render m.active }
            , if m.sketchy then
                sketchy Sketchy.defaultConfig (render m.active)
                    |> Random.generate GeneratedSketchy
                else
                    Cmd.none
            )
    in
  case msg of
    Switch ->
        { model | active = not model.active }
            |> renderModel

    ClickedNormal ->
        { model | sketchy = False }
            |> renderModel

    ClickedSketchy ->
        { model | sketchy = True }
            |> renderModel

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


elps : Bool -> Collage Msg
elps active =
  ellipse 100 50
    |> styled
        ( uniform <|
            if active then
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

render active =
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
    , stack [ showEnvelope txt, elps active ]
    ]


button : String -> Msg -> Html Msg
button name msg =
    Html.button [ Html.Events.onClick msg ] [ Html.text name ]


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div []
            [ button "Normal" ClickedNormal
            , button "Sketchy" ClickedSketchy
            ]
        , model.collage |> svg
        ]


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }

