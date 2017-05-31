module Collage.Core
    exposing
        ( Point
        , Collage
        , BasicCollage(..)
        , Path(..)
        , Shape(..)
        , ShapeStyle
        , FillStyle(..)
        , LineStyle
        , LineCap(..)
        , LineJoin(..)
        )

import Html exposing (Html)
import Color exposing (Color)
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
    = Shape ShapeStyle Shape
    | Path LineStyle Path
      -- | Text Text TextAlign
    | Image String Float Float
    | Group (List (Collage msg))
    | Element (Html msg)


type Shape
    = Polygon (List Point)
    | Ellipse Float Float


type alias ShapeStyle =
    { fill : FillStyle
    , line : LineStyle
    }


type Path
    = Polyline (List Point)


type FillStyle
    = Transparent
    | Uniform Color


type alias LineStyle =
    { fill : FillStyle
    , thickness : Float
    , cap : LineCap
    , join : LineJoin
    , dashing : List ( Int, Int )
    , dashOffset : Int
    }


type LineCap
    = Flat
    | Round
    | Padded


type LineJoin
    = Smooth
    | Sharp
    | Clipped
