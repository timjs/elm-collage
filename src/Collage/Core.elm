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
        , find
        , foldl
        , foldr
        , foldrLazy
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


type alias Collage fill line text msg =
    { name : Maybe String
    , origin : Point
    , theta : Float
    , scale : Float
    , alpha : Float
    , handlers : List ( String, Json.Decoder msg )
    , basic : BasicCollage fill line text msg
    }


type BasicCollage fill line text msg
    = Shape ( fill, line ) Shape
    | Path line Path
    | Text ( Float, Float ) (Text text)
    | Image ( Float, Float ) String
    | Html ( Float, Float ) (Html msg)
    | Group (List (Collage fill line text msg))
    | Subcollage (Collage fill line text msg) (Collage fill line text msg)


collage : BasicCollage fill line text msg -> Collage fill line text msg
collage basic =
    { name = Nothing
    , origin = ( 0, 0 )
    , theta = 0
    , scale = 1
    , alpha = 1
    , handlers = []
    , basic = basic
    }


apply : { r | origin : Point, theta : Float, scale : Float } -> Point -> Point
apply { origin, theta, scale } pt =
    let
        ( tx, ty ) =
            origin

        scaled ( x, y ) =
            ( x * scale, y * scale )

        shifted ( x, y ) =
            ( x + tx, y + ty )

        rotated ( x, y ) =
            let
                c =
                    cos theta

                s =
                    sin theta
            in
            ( c * x - s * y, s * x + c * y )
    in
    pt |> rotated |> shifted |> scaled


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
