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
        )

{-| This module contains internal types used accross multiple modules in this packages.
Constructors are however not exposed to the user.
-}

import Color exposing (Color)
import Html exposing (Html)
import Json.Decode as Json


-- Point -----------------------------------------------------------------------


type alias Point =
    ( Float, Float )



-- Collage ---------------------------------------------------------------------


type alias Transform r =
    { r | shift : ( Float, Float ), scale : ( Float, Float ), rotation : Float }


type alias Collage fill line text msg =
    Transform
        { opacity : Float
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
    { shift = ( 0, 0 )
    , scale = ( 1, 1 )
    , rotation = 0
    , opacity = 1
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
