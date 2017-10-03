module Collage.Core
    exposing
        ( BasicCollage(..)
        , Collage
        , FillStyle(..)
        , LineCap(..)
        , LineJoin(..)
        , LineStyle
        , Path(..)
        , Point
        , Shape(..)
        , collage
        )

{-| This module contains internal types used accross multiple modules in this packages.
Constructors are however not exposed to the user.
-}

import Collage.Text exposing (Text)
import Color exposing (Color)
import Html exposing (Html)
import Json.Decode as Json


-- Point -----------------------------------------------------------------------


type alias Point =
    ( Float, Float )



-- Collage ---------------------------------------------------------------------


type alias Collage msg =
    { origin : Point
    , theta : Float
    , scale : Float
    , alpha : Float
    , handlers : List ( String, Json.Decoder msg )
    , basic : BasicCollage msg
    }


collage : BasicCollage msg -> Collage msg
collage basic =
    { origin = ( 0, 0 )
    , theta = 0
    , scale = 1
    , alpha = 1
    , handlers = []
    , basic = basic
    }


type BasicCollage msg
    = Shape ( FillStyle, LineStyle ) Shape
    | Path LineStyle Path
    | Text ( Float, Float ) Text
    | Image ( Float, Float ) String
    | Element ( Float, Float ) (Html msg)
    | Group (List (Collage msg))
    | Subcollage (Collage msg) (Collage msg)



-- Shapes, Paths and Text ------------------------------------------------------


type Shape
    = Polygon (List Point)
      --TODO: Although Rectangles are a special case of Polygons, they can have rounded corners,
      -- | Rectangle Float Float
      --NOTE: Squares are just Rectangles with the same width and height, therefore we don't need them here.
      --NOTE: Circles are just Elipses with the same x- and y-radius, therefore we don't need them here.
    | Ellipse Float Float
    | Loop Path


type Path
    = Polyline (List Point)



-- Styles ----------------------------------------------------------------------
-- Fill style -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


type FillStyle
    = Transparent
      --TODO: Add gradient and pattern fills
      -- | Gradient Gradient
      -- | Pattern Float Float String Float
    | Uniform Color



-- Line style -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


type alias LineStyle =
    { fill : FillStyle
    , thickness : Float
    , cap : LineCap
    , join : LineJoin
    , dashPattern : List ( Int, Int )
    , dashPhase : Int
    }


{-| In Tikz and Svg:

    = Butt
    | Round
    | Rect

-}
type LineCap
    = Flat
    | Round
    | Padded


{-| In Tikz and Svg:

    = Round
    | Bevel
    | Miter

-}
type LineJoin
    = Smooth
    | Clipped
    | Sharp
