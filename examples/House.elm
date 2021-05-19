module House exposing (house, main)

import Browser
import Collage exposing (..)
import Collage.Events exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (..)
import Collage.Sketchy exposing (sketchy)
import Color exposing (..)
import Html exposing (Html)
import Random



-- Model -----------------------------------------------------------------------


type alias Model =
  { hover : Part, collage: Collage Msg }


type Part
  = None
  | Roof
  | Chimney
  | Smoke
  | Wall
  | Door
  | Handle


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            Model None (group [])
    in
    ( model, sketchy (house model) |> Random.generate GeneratedSketchy )



-- Update ----------------------------------------------------------------------


type Msg
  = ChangePart Part
  | GeneratedSketchy (Collage Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangePart part ->
            let
                newModel =
                    { model | hover = part }
            in
            ( newModel, sketchy (house newModel) |> Random.generate GeneratedSketchy )

        GeneratedSketchy collage ->
            ( { model | collage = collage }, Cmd.none )



-- View ------------------------------------------------------------------------


house : Model -> Collage Msg
house model =
  let
    interactive : Part -> FillStyle -> Shape -> Collage Msg
    interactive part fill shape =
      shape
        |> filled
            (if model.hover == part then
              uniform purple
             else
              fill
            )
        |> onMouseEnter (always <| ChangePart part)
    --TODO: add `lengthen 0.75`
    roof = interactive Roof (uniform blue) (triangle 1)
    door = interactive Door (uniform red) (rectangle 0.2 0.4)
    handle = interactive Handle (uniform black) (circle 0.02)
    wall = interactive Wall (uniform yellow) (square 1)
    chimney = interactive Chimney (uniform green) (rectangle 0.1 0.4)
    smoke =
      let
        puff p =
          interactive Smoke (uniform gray) (circle 0.05)
            |> shift p
        puffs = List.map puff [ ( 0, 0 ), ( 0.05, 0.15 ) ]
      in
      stack puffs
  in
  vertical
    [ stack
        [ roof
        , chimney
            |> at (top >> (\( x, y ) -> ( x, y + 0.15 ))) smoke
            |> shift ( 0.25, 0 )
        ]
        |> center
    , stack
        [ handle |> shift ( 0.05, 0.2 )
        , door |> align bottom
        , wall |> align bottom
        ]
    ]


view : Model -> Html Msg
view model =
  model.collage
    |> scale 200
    |> svg



-- Main ------------------------------------------------------------------------


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


{- Compare https://archives.haskell.org/projects.haskell.org/diagrams/blog/2015-04-30-GTK-coordinates.html:

   prettyHouse :: QDiagram Cairo V2 Double [String]
   prettyHouse = house
     where
       roof    = triangle 1   # scaleToY 0.75 # centerY # fc blue
       door    = rect 0.2 0.4 # fc red
       handle  = circle 0.02  # fc black
       wall    = square 1     # fc yellow
       chimney = fromOffsets [0 ^& 0.25, 0.1 ^& 0, 0 ^& (-0.4)]
               # closeTrail # strokeT # fc green
               # centerX
               # named "chimney"
       smoke = mconcat
         [ circle 0.05 # translate v
         | v <- [ zero, 0.05 ^& 0.15 ]
         ]
         # fc grey
       house = vcat
         [ mconcat
           [ roof    # snugR                  # value ["roof"]
           , chimney # snugL                  # value ["chimney"]
           ]
           # centerX
         , mconcat
           [ handle  # translate (0.05 ^& 0.2) # value ["handle"]
           , door    # alignB                  # value ["door"]
           , wall    # alignB                  # value ["wall"]
           ]
         ]
         # withName "chimney" (\chim ->
             atop (smoke # moveTo (location chim) # translateY 0.4
                         # value ["smoke"]
                  )
           )
-}
