module Composition exposing (main)

import Collage exposing (..)
import Collage.Events exposing (onClick)
import Collage.Layout exposing (..)
import Collage.Text exposing (fromString)
import Color exposing (..)
import Example


-- Model -----------------------------------------------------------------------


type alias Model =
  { active : Bool }



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
    , stack [ showEnvelope txt, elps model.active ]
    ]


main : Platform.Program () (Example.Model Msg Model) (Example.Msg Msg)
main =
    Example.example
        { init = Model False
        , update = update
        , render = render
        , view = identity
        }
