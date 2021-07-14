module House exposing (house, main)

import Collage exposing (..)
import Example
import Collage.Events exposing (..)
import Collage.Layout exposing (..)
import Color exposing (..)
import Html exposing (Html)



-- Model -----------------------------------------------------------------------


type alias Model =
  { hover : Part }


type Part
  = None
  | Roof
  | Chimney
  | Smoke
  | Wall
  | Door
  | Handle


-- Update ----------------------------------------------------------------------


type Msg
  = ChangePart Part


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangePart part ->
            { model | hover = part }



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
    roof = interactive Roof (uniform blue) (triangle 100)
    door = interactive Door (uniform red) (rectangle 20 40)
    handle = interactive Handle (uniform black) (circle 2)
    wall = interactive Wall (uniform yellow) (square 100)
    chimney = interactive Chimney (uniform green) (rectangle 10 40)
    smoke =
      let
        puff p =
          interactive Smoke (uniform gray) (circle 5)
            |> shift p
        puffs = List.map puff [ ( 0, 0 ), ( 5, 15 ) ]
      in
      stack puffs
  in
  vertical
    [ stack
        [ roof
        , chimney
            |> at (top >> (\( x, y ) -> ( x, y + 15 ))) smoke
            |> shift ( 25, 0 )
        ]
        |> center
    , stack
        [ handle |> shift ( 5, 20 )
        , door |> align bottom
        , wall |> align bottom
        ]
    ]


-- Main ------------------------------------------------------------------------


main : Platform.Program () (Example.Model Msg Model) (Example.Msg Msg)
main =
    Example.example
        { init = Model None
        , update = update
        , render = house >> (scale 2)
        , view = identity
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
