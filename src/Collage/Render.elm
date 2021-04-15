module Collage.Render exposing (svg, svgBox, svgExplicit)

{-| Technically, it should be possible to use different backends to render a collage,
but we only provide a Svg backend here.

@docs svg, svgBox, svgExplicit

-}

import Basics.Extra exposing (uncurry)
import Collage exposing (Collage, Point)
import Collage.Core as Core
import Collage.Layout as Layout
import Collage.Text as Text exposing (Text)
import Color exposing (Color)
import Html exposing (Html)
import Json.Decode as Json
import List
import Maybe exposing (withDefault)
import String exposing (fromFloat, fromInt)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as Svg
import Svg.Events as Svg
import Tuple


{-| Render a collage as Svg in a view box of given width and height,
and the origin in the center.

_Maybe this will be removed from the next major version.
Please open an issue if you want to keep this._

-}
svgBox : ( Float, Float ) -> Collage msg -> Html msg
svgBox ( width, height ) collage =
  svgAbsolute ( width, height ) <|
    Collage.shift ( width / 2, -height / 2 ) collage


{-| Take a collage and render it to Html using Svg.

It uses the automatically calculated envelope from the Collage.Layout module as the view box.

-}
svg : Collage msg -> Html msg
svg collage =
  svgAbsolute ( Layout.width collage, Layout.height collage ) <|
    Layout.align Layout.topLeft collage


{-| Take a collage and render it to Html using Svg
explicitly specifying the HTML attributes of the element.
-}
svgExplicit : List (Attribute msg) -> Collage msg -> Html msg
svgExplicit attributes collage =
  Svg.svg attributes [ render collage ]


svgAbsolute : ( Float, Float ) -> Collage msg -> Html msg
svgAbsolute ( width, height ) collage =
  let
    w = fromFloat width
    h = fromFloat height
  in
  Html.div
    []
    [ Svg.svg
        [ Svg.width w
        , Svg.height h
        , Svg.version "1.1"
        ]
        [ render collage ]
    ]


render : Collage msg -> Svg msg
render collage =
  let
    name = collage.name |> withDefault "_unnamed_"
  in
  case collage.basic of
    Core.Path style path ->
      case path of
        Core.Polyline ps ->
          Svg.polyline
            ([ Svg.id name
             , Svg.points <| decodePoints ps
             ]
              ++ attrs collage
              ++ events collage.handlers
            )
            []
    Core.Shape ( fill, line ) shape ->
      case shape of
        Core.Polygon ps ->
          Svg.polygon
            ([ Svg.id name
             , Svg.points <| decodePoints ps
             ]
              ++ attrs collage
              ++ events collage.handlers
            )
            []
        Core.Circle r ->
          Svg.circle
            ([ Svg.id name
             , Svg.r <| fromFloat r
             ]
              ++ attrs collage
              ++ events collage.handlers
            )
            []
        Core.Ellipse rx ry ->
          Svg.ellipse
            ([ Svg.id name
             , Svg.rx <| fromFloat rx
             , Svg.ry <| fromFloat ry
             ]
              ++ attrs collage
              ++ events collage.handlers
            )
            []
        Core.Rectangle w h r ->
          Svg.rect
            ([ Svg.id name
             , Svg.rx <| fromFloat r
             , Svg.ry <| fromFloat r
             ]
              ++ box w h
              ++ attrs collage
              ++ events collage.handlers
            )
            []
        Core.Loop path ->
          --NOTE: Use the same rendering as for a path
          render { collage | basic = Core.Path line path }
    Core.Text _ (Core.Chunk style str) ->
      Svg.text_
        ([ Svg.id name ]
          ++ attrs collage
          ++ events collage.handlers
        )
        [ Svg.text str ]
    Core.Image ( w, h ) url ->
      Svg.image
        ([ Svg.id name
         , Svg.xlinkHref url
         ]
          ++ box w h
          ++ attrs collage
          ++ events collage.handlers
        )
        []
    Core.Html ( w, h ) extraAttrs html ->
      Svg.foreignObject
        ([ Svg.id name ]
          ++ box w h
          ++ attrs collage
          ++ events collage.handlers
          ++ extraAttrs
        )
        [ html ]
    Core.Group collages ->
      --NOTE: Order of collages is reversed here! Svg renders group elements from back to front.
      Svg.g (Svg.id name :: attrs collage ++ events collage.handlers) <|
        List.foldl (\col res -> render col :: res) [] collages
    Core.Subcollage fore back ->
      --NOTE: Rendering a subcollage is the same as rendering a group, only layout calculations in `Collage.Layout` differ.
      render { collage | basic = Core.Group [ fore, back ] }


box : Float -> Float -> List (Attribute msg)
box w h =
  [ Svg.width <| fromFloat w
  , Svg.height <| fromFloat h
  , Svg.x <| fromFloat (-w / 2)
  , Svg.y <| fromFloat (-h / 2)
  ]


events : List ( String, Json.Decoder msg ) -> List (Attribute msg)
events handlers =
  List.map (uncurry Svg.on) handlers


attrs : Collage msg -> List (Attribute msg)
attrs collage =
  case collage.basic of
    Core.Path line _ ->
      [ Svg.stroke <| decodeFill line.fill
      , Svg.strokeOpacity <| decodeFillOpacity line.fill
      , Svg.strokeWidth <| fromFloat line.thickness
      , Svg.strokeLinecap <| decodeCap line.cap
      , Svg.strokeLinejoin <| decodeJoin line.join
      , Svg.fill <| "none"
      , Svg.opacity <| fromFloat collage.opacity
      , Svg.transform <| decodeTransform collage
      , Svg.strokeDashoffset <| fromInt line.dashPhase
      , Svg.strokeDasharray <| decodeDashing line.dashPattern
      ]
    Core.Shape ( fill, line ) _ ->
      [ Svg.fill <| decodeFill fill
      , Svg.fillOpacity <| decodeFillOpacity fill
      , Svg.stroke <| decodeFill line.fill
      , Svg.strokeOpacity <| decodeFillOpacity line.fill
      , Svg.strokeWidth <| fromFloat line.thickness
      , Svg.strokeLinecap <| decodeCap line.cap
      , Svg.strokeLinejoin <| decodeJoin line.join
      , Svg.opacity <| fromFloat collage.opacity
      , Svg.transform <| decodeTransform collage
      , Svg.strokeDashoffset <| fromInt line.dashPhase
      , Svg.strokeDasharray <| decodeDashing line.dashPattern
      ]
    Core.Text _ (Core.Chunk style str) ->
      [ Svg.fill <| decodeFill (Core.Uniform style.color)
      , Svg.fontFamily <|
          case style.typeface of
            Text.Serif -> "serif"
            Text.Sansserif -> "sans-serif"
            Text.Monospace -> "monospace"
            Text.Font name -> name
      , Svg.fontSize <| fromInt style.size
      , Svg.fontWeight <|
          case style.weight of
            Text.Thin -> "200"
            Text.Light -> "300"
            Text.Regular -> "normal"
            Text.Medium -> "500"
            Text.SemiBold -> "600"
            Text.Bold -> "bold"
            Text.Black -> "800"
      , Svg.fontStyle <|
          case style.shape of
            Text.Upright -> "normal"
            Text.SmallCaps -> "normal"
            Text.Slanted -> "oblique"
            Text.Italic -> "italic"
      , Svg.fontVariant <|
          case style.shape of
            Text.SmallCaps -> "small-caps"
            _ -> "normal"
      , Svg.textDecoration <|
          case style.line of
            Text.None -> "none"
            Text.Under -> "underline"
            Text.Over -> "overline"
            Text.Through -> "line-through"
      , Svg.textAnchor <| "middle"
      , Svg.dominantBaseline "middle"
      , Svg.opacity <| fromFloat collage.opacity
      , Svg.transform <| decodeTransform collage
      ]
    _ ->
      [ Svg.opacity <| fromFloat collage.opacity
      , Svg.transform <| decodeTransform collage
      ]


decodeCap : Collage.LineCap -> String
decodeCap cap =
  case cap of
    Collage.Round -> "round"
    Collage.Padded -> "square"
    Collage.Flat -> "butt"


decodeJoin : Collage.LineJoin -> String
decodeJoin join =
  case join of
    Collage.Smooth -> "round"
    Collage.Sharp -> "miter"
    Collage.Clipped -> "bevel"


decodePoints : List Point -> String
decodePoints ps =
  ps |> List.map (\( x, y ) -> String.join "," [ fromFloat x, fromFloat -y ]) |> String.join " "


decodeTransform : Collage msg -> String
decodeTransform collage =
  let
    dx = fromFloat <| Tuple.first collage.shift
    dy = fromFloat <| -(Tuple.second collage.shift)
    r = fromFloat <| -collage.rotation / 2 / pi * 360
    sx = fromFloat <| Tuple.first collage.scale
    sy = fromFloat <| Tuple.second collage.scale
  in
  String.concat
    [ "translate(", dx, ",", dy, ") scale(", sx, ",", sy, ") rotate(", r, ")" ]


decodeFill : Core.FillStyle -> String
decodeFill fs =
  case fs of
    Core.Uniform c -> decodeColor c
    Core.Transparent -> "none"


decodeFillOpacity : Core.FillStyle -> String
decodeFillOpacity fs =
  case fs of
    Core.Uniform c -> decodeOpacity c
    Core.Transparent -> "0"


decodeColor : Color -> String
decodeColor c =
  let
    { red, green, blue } = Color.toRgba c
  in
  Color.rgb red green blue
    |> Color.toCssString


decodeOpacity : Color -> String
decodeOpacity c =
  let
    { alpha } = Color.toRgba c
  in
  fromFloat alpha


decodeDashing : List ( Int, Int ) -> String
decodeDashing ds =
  let
    decodeOnOff ( x, y ) =
      String.join "," [ fromInt x, fromInt y ]
  in
  ds
    |> List.map decodeOnOff
    |> String.join " "
