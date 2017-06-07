module Collage.Core
    exposing
        ( Point
        , Collage
        , BasicCollage(..)
        , Path(..)
        , Shape(..)
        , FillStyle(..)
        , LineStyle
        , LineCap(..)
        , LineJoin(..)
        )

{-| This module contains internal types used accross multiple modules in this packages.
Constructors are however not exposed to the user.
-}

import Html exposing (Html)
import Color exposing (Color)
import Text exposing (Text)
import Json.Decode as Json


type alias Point =
    ( Float, Float )


type alias Collage msg =
    { origin : Point
    , theta : Float
    , scale : Float
    , alpha : Float
    , basic : BasicCollage msg
    , handlers : List ( String, Json.Decoder msg )
    }


type BasicCollage msg
    = Shape ( FillStyle, LineStyle ) Shape
    | Path LineStyle Path
    | Text Text
    | Image Float Float String
    | Element Float Float (Html msg)
    | Group (List (Collage msg))


type Shape
    = Polygon (List Point)
      -- Although Rectangles are a special case of Polygons, they can have rounded corners,
      -- Squares are just Rectangles with the same width and height, therefore we don't need them here.
      -- | Rectangle Float Float
      -- Circles are just Elipses with the same x- and y-radius, therefore we don't need them here.
    | Ellipse Float Float


type Path
    = Polyline (List Point)


type FillStyle
    = Transparent
      -- | Gradient Gradient
      -- | Pattern Float Float String Float
      -- | FillStyle String
    | Uniform Color


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
