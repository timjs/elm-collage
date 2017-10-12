module Collage.Core
    exposing
        ( BasicCollage(..)
        , Collage
        , FillStyle(..)
        , Path(..)
        , Shape(..)
        , Text(..)
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


type alias Collage fill line text msg =
    { origin : Point
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
    { origin = ( 0, 0 )
    , theta = 0
    , scale = 1
    , alpha = 1
    , handlers = []
    , basic = basic
    }



-- Shapes, Paths and Text ------------------------------------------------------


type Shape
    = Polygon (List Point)
      --TODO: Although Rectangles are a special case of Polygons, they can have rounded corners,
      --NOTE: Squares are just Rectangles with the same width and height, therefore we don't need them here.
    | Rectangle Float Float Float
    | Ellipse Float Float
      --NOTE: Circles are just Elipses with the same x- and y-radius, so we could just use Ellipse,
      --NOTE: but it eases the calculation of envelopes...
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
