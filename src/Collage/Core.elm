module Collage.Core
    exposing
        ( BasicCollage(..)
        , Collage
        , FillStyle(..)
        , Path(..)
        , Shape(..)
        , Text(..)
        , apply
        , collage
        , combine
        , find
        , foldl
        , foldr
        , foldrLazy
        , levels
        , search
        )

{-| This module contains internal types used accross multiple modules in this packages.
Constructors are however not exposed to the user.
-}

import Color exposing (Color)
import Helpers
import Html exposing (Html)
import Json.Decode as Json
import Maybe.Extra as Maybe


-- Point -----------------------------------------------------------------------


type alias Point =
    ( Float, Float )



-- Collage ---------------------------------------------------------------------


type alias Transform r =
    { r | shift : ( Float, Float ), scale : ( Float, Float ), rotation : Float }


type alias Collage fill line text msg =
    Transform
        { opacity : Float
        , name : Maybe String
        , handlers : List ( String, Json.Decoder msg )
        , basic : BasicCollage fill line text msg
        }


type BasicCollage fill line text msg
    = Shape ( fill, line ) Shape
    | Path line Path
    | Text ( Float, Float ) (Text text)
    | Image ( Float, Float ) String
    | Html ( Float, Float ) (Html msg)
      --FIXME: Implement grouping as fold over stacking?
    | Group (List (Collage fill line text msg))
    | Subcollage (Collage fill line text msg) (Collage fill line text msg)


collage : BasicCollage fill line text msg -> Collage fill line text msg
collage basic =
    { shift = ( 0, 0 )
    , scale = ( 1, 1 )
    , rotation = 0
    , opacity = 1
    , name = Nothing
    , handlers = []
    , basic = basic
    }


apply : Transform r -> Point -> Point
apply { shift, scale, rotation } =
    let
        ( dx, dy ) =
            shift

        ( sx, sy ) =
            scale

        shifted ( x, y ) =
            ( x + dx, y + dy )

        scaled ( x, y ) =
            ( sx * x, sy * y )

        rotated ( x, y ) =
            let
                c =
                    cos rotation

                s =
                    sin rotation
            in
            ( c * x - s * y, s * x + c * y )
    in
    shifted << scaled << rotated


combine : Transform r -> Transform r -> Transform r
combine { shift, scale, rotation } this =
    let
        ( dx, dy ) =
            shift

        ( fx, fy ) =
            scale

        ( x, y ) =
            this.shift

        ( sx, sy ) =
            this.shift
    in
    { this
        | shift = ( x + dx, y + dy )
        , scale = ( sx * fx, sy * fy )
        , rotation = this.rotation + rotation
    }


foldr : (Collage fill line text msg -> a -> a) -> a -> Collage fill line text msg -> a
foldr f acc collage =
    let
        foldrOf =
            List.foldr (\collage acc -> foldr f acc collage) acc

        recurse =
            case collage.basic of
                Group collages ->
                    foldrOf collages

                Subcollage fore back ->
                    foldrOf [ fore, back ]

                _ ->
                    acc
    in
    f collage recurse


foldrLazy : (Collage fill line text msg -> (() -> a) -> a) -> a -> Collage fill line text msg -> a
foldrLazy f acc collage =
    let
        foldrOf =
            Helpers.foldrLazy (\collage acc -> foldrLazy f (acc ()) collage) acc

        recurse () =
            case collage.basic of
                Group collages ->
                    foldrOf collages

                Subcollage fore back ->
                    foldrOf [ fore, back ]

                _ ->
                    acc
    in
    f collage recurse


foldl : (Collage fill line text msg -> a -> a) -> a -> Collage fill line text msg -> a
foldl f acc collage =
    let
        foldlOf acc =
            List.foldl (\collage acc -> foldl f acc collage) acc

        recurse acc =
            case collage.basic of
                Group collages ->
                    foldlOf acc collages

                Subcollage fore back ->
                    foldlOf acc [ fore, back ]

                _ ->
                    acc
    in
    recurse <| f collage acc


{-| Lazy depth-first search using `foldr`
-}
find : (Collage fill line text msg -> Bool) -> Collage fill line text msg -> Maybe (Collage fill line text msg)
find p =
    --NOTE: Could be defined generically on types having `foldr`.
    let
        f x =
            if p x then
                Just x
            else
                Nothing
    in
    foldrLazy (Maybe.orLazy << f) Nothing


levels : Collage fill line text msg -> List (Collage fill line text msg)
levels collage =
    let
        recurse result queue =
            --NOTE: This function is tail recursive :-)
            case queue of
                [] ->
                    List.reverse result

                collage :: rest ->
                    case collage.basic of
                        Group collages ->
                            --NOTE: First recurse on the rest of the queue, then go for the group contents
                            recurse result (rest ++ collages)

                        Subcollage fore back ->
                            recurse result (rest ++ [ fore, back ])

                        _ ->
                            --NOTE: We only add non-groups to the result
                            recurse (collage :: result) rest
    in
    --NOTE: Start with the empty queue as the result and the current collage in the queue
    recurse [] [ collage ]


{-| Breadth-first search on collages
-}
search : (Collage fill line text msg -> Bool) -> Collage fill line text msg -> Maybe (Collage fill line text msg)
search pred collage =
    let
        recurse queue =
            case queue of
                [] ->
                    Nothing

                collage :: rest ->
                    if pred collage then
                        --NOTE: We found it!
                        Just collage
                    else
                        --NOTE: We go on with our search
                        case collage.basic of
                            Group collages ->
                                --NOTE: First recurse on the rest of the queue, then go for the group contents
                                recurse (rest ++ collages)

                            Subcollage fore back ->
                                recurse (rest ++ [ fore, back ])

                            _ ->
                                recurse rest
    in
    recurse [ collage ]



-- Shapes, Paths and Text ------------------------------------------------------


type Shape
    = Polygon (List Point)
      --NOTE: Although Rectangles are a special case of Polygons, they can have rounded corners, so we have a separate constructor for them.
      --NOTE: Squares are just Rectangles with the same width and height, therefore we don't need them here.
    | Rectangle Float Float Float
    | Ellipse Float Float
      --NOTE: Circles are just Elipses with the same x- and y-radius, so we could just use Ellipse, but it eases the calculation of envelopes.
    | Circle Float
    | Loop Path


type Path
    = Polyline (List Point)


type Text style
    = Chunk style String



-- Styles ----------------------------------------------------------------------


type FillStyle
    = Transparent
      --TODO: Add gradient and pattern fills
      -- | Gradient Gradient
      -- | Pattern Float Float String Float
    | Uniform Color
