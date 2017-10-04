module House exposing (house, main)

import Collage exposing (..)
import Collage.Events exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (..)
import Color exposing (..)
import Html exposing (Html)


-- Model -----------------------------------------------------------------------


type alias Model =
    { active : Part }


type Part
    = None
    | Roof
    | Chimney
    | Smoke
    | Wall
    | Door
    | Handle


init =
    { active = None }



-- Update ----------------------------------------------------------------------


type alias Msg =
    Part


update : Msg -> Model -> Model
update msg model =
    { active = msg }



-- View ------------------------------------------------------------------------


roof model =
    triangle 1
        |> filled
            (if model.active == Roof then
                uniform purple
             else
                uniform blue
            )
        |> onMouseEnter (always Roof)



--TODO: add broaden
-- |> broaden 0.75


door model =
    rectangle 0.2 0.4
        |> filled
            (if model.active == Door then
                uniform purple
             else
                uniform red
            )
        |> onMouseEnter (always Door)


handle model =
    circle 0.02
        |> filled
            (if model.active == Handle then
                uniform purple
             else
                uniform black
            )
        |> onMouseEnter (always Handle)


wall model =
    square 1
        |> filled
            (if model.active == Wall then
                uniform purple
             else
                uniform yellow
            )
        |> onMouseEnter (always Wall)


chimney model =
    rectangle 0.1 0.4
        |> filled
            (if model.active == Chimney then
                uniform purple
             else
                uniform green
            )
        |> onMouseEnter (always Chimney)


smoke model =
    let
        puff p =
            circle 0.05
                |> filled
                    (if model.active == Smoke then
                        uniform purple
                     else
                        uniform gray
                    )
                |> shift p
                |> onMouseEnter (always Smoke)

        puffs =
            List.map puff [ ( 0, 0 ), ( 0.05, -0.15 ) ]
    in
    stack puffs


house model =
    vertical
        [ stack
            [ roof model
            , chimney model
                |> at (top >> (\( x, y ) -> ( x, y + 0.15 ))) (smoke model)
                |> shift ( 0.25, -0.4 )
            ]
            |> center
        , stack
            [ handle model |> shift ( 0.05, -0.2 )
            , door model |> align bottom
            , wall model |> align bottom
            ]
        ]


view : Model -> Html Msg
view model =
    house model
        |> scale 100
        |> svg



-- Main ------------------------------------------------------------------------


main =
    Html.beginnerProgram
        { model = init
        , view = view
        , update = update
        }



{-
   -- The diagram to be drawn, with features tagged by strings.
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
