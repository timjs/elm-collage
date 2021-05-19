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
import Array exposing (Array)
import Svg.Attributes as SvgA
import Svg.Events as SvgE
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
        [ SvgA.width w
        , SvgA.height h
        , SvgA.version "1.1"
        ]
        [ render collage ]
    ]


-- FROM https://github.com/rough-stuff/rough/blob/e9b0fdf36952a7a0f02e8015f4abac1ad39981c5/src/renderer.ts#L367
bCurvePath : List Point -> String
bCurvePath ps =
    let
        neighbors arr index =
            Maybe.map4
                (\m1 p1 i p2 ->
                    ((m1, p1), (i, p2))
                )
                (Array.get (index - 1) arr)
                (Array.get (index + 1) arr)
                (Array.get index arr)
                (Array.get (index + 2) arr)
    in
    case ps of
        [] ->
            ""

        [ p ] ->
            ""

        [ (x1, y1), (x2, y2) ] ->
            [ "M", [ x1, y1 ] |> List.map (String.fromFloat) |> String.join " "
            , "L", [ x2, y2 ] |> List.map (String.fromFloat) |> String.join " "
            ] |> String.join " "

        (x1, y1) :: tail ->
           let
               last = List.reverse tail |> List.take 1
               arr = Array.fromList ((x1, y1) :: (x1, y1) :: tail ++ last)
               curves =
                    Array.indexedMap (\i p ->
                       case (neighbors arr i) of
                            Just (((m1x, m1y), (p1x, p1y)), ((ix, iy), (p2x, p2y))) ->
                                [ "C", [ ix + (p1x - m1x) / 3, iy + (p1y - m1y) / 3  ]
                                    |> List.map (String.fromFloat)
                                    |> String.join " "
                                , ",", [ p1x + (p2x - ix) / 3, p1y + (p2y - iy) / 3  ]
                                    |> List.map (String.fromFloat)
                                    |> String.join " "
                                , ",", [ p1x, p1y ]
                                    |> List.map (String.fromFloat)
                                    |> String.join " "
                                ] |> String.join " "
                            Nothing ->
                                ""
                        ) arr
                        |> Array.toList

           in
           ([ "M", String.fromFloat x1, String.fromFloat y1 ] ++ curves) |> String.join " "


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
            ([ SvgA.id name
             , SvgA.points <| decodePoints ps
             ]
              ++ attrs collage
              ++ events collage.handlers
            )
            []

        Core.Curve ps ->
            Svg.path
                ([ SvgA.id name
                 , SvgA.d (bCurvePath (ps |> List.map (\(x1, y1) -> (x1, -y1))))
                 ]
                  ++ attrs collage
                  ++ events collage.handlers
                )
                []

    Core.Shape ( fill, line ) shape ->
      case shape of
        Core.Polygon ps ->
          Svg.polygon
            ([ SvgA.id name
             , SvgA.points <| decodePoints ps
             ]
              ++ attrs collage
              ++ events collage.handlers
            )
            []
        Core.Circle r ->
          Svg.circle
            ([ SvgA.id name
             , SvgA.r <| fromFloat r
             ]
              ++ attrs collage
              ++ events collage.handlers
            )
            []
        Core.Ellipse rx ry ->
          Svg.ellipse
            ([ SvgA.id name
             , SvgA.rx <| fromFloat rx
             , SvgA.ry <| fromFloat ry
             ]
              ++ attrs collage
              ++ events collage.handlers
            )
            []
        Core.Rectangle w h r ->
          Svg.rect
            ([ SvgA.id name
             , SvgA.rx <| fromFloat r
             , SvgA.ry <| fromFloat r
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
        ([ SvgA.id name ]
          ++ attrs collage
          ++ events collage.handlers
        )
        [ Svg.text str ]
    Core.Image ( w, h ) url ->
      Svg.image
        ([ SvgA.id name
         , SvgA.xlinkHref url
         ]
          ++ box w h
          ++ attrs collage
          ++ events collage.handlers
        )
        []
    Core.Html ( w, h ) extraAttrs html ->
      Svg.foreignObject
        ([ SvgA.id name ]
          ++ box w h
          ++ attrs collage
          ++ events collage.handlers
          ++ extraAttrs
        )
        [ html ]
    Core.Group collages ->
      --NOTE: Order of collages is reversed here! Svg renders group elements from back to front.
      Svg.g (SvgA.id name :: attrs collage ++ events collage.handlers) <|
        List.foldl (\col res -> render col :: res) [] collages
    Core.Subcollage fore back ->
      --NOTE: Rendering a subcollage is the same as rendering a group, only layout calculations in `Collage.Layout` differ.
      render { collage | basic = Core.Group [ fore, back ] }


box : Float -> Float -> List (Attribute msg)
box w h =
  [ SvgA.width <| fromFloat w
  , SvgA.height <| fromFloat h
  , SvgA.x <| fromFloat (-w / 2)
  , SvgA.y <| fromFloat (-h / 2)
  ]


events : List ( String, Json.Decoder msg ) -> List (Attribute msg)
events handlers =
  List.map (uncurry SvgE.on) handlers


attrs : Collage msg -> List (Attribute msg)
attrs collage =
  case collage.basic of
    Core.Path line _ ->
      [ SvgA.stroke <| decodeFill line.fill
      , SvgA.strokeOpacity <| decodeFillOpacity line.fill
      , SvgA.strokeWidth <| fromFloat line.thickness
      , SvgA.strokeLinecap <| decodeCap line.cap
      , SvgA.strokeLinejoin <| decodeJoin line.join
      , SvgA.fill <| "none"
      , SvgA.opacity <| fromFloat collage.opacity
      , SvgA.transform <| decodeTransform collage
      , SvgA.strokeDashoffset <| fromInt line.dashPhase
      , SvgA.strokeDasharray <| decodeDashing line.dashPattern
      ]
    Core.Shape ( fill, line ) _ ->
      [ SvgA.fill <| decodeFill fill
      , SvgA.fillOpacity <| decodeFillOpacity fill
      , SvgA.stroke <| decodeFill line.fill
      , SvgA.strokeOpacity <| decodeFillOpacity line.fill
      , SvgA.strokeWidth <| fromFloat line.thickness
      , SvgA.strokeLinecap <| decodeCap line.cap
      , SvgA.strokeLinejoin <| decodeJoin line.join
      , SvgA.opacity <| fromFloat collage.opacity
      , SvgA.transform <| decodeTransform collage
      , SvgA.strokeDashoffset <| fromInt line.dashPhase
      , SvgA.strokeDasharray <| decodeDashing line.dashPattern
      ]
    Core.Text _ (Core.Chunk style str) ->
      [ SvgA.fill <| decodeFill (Core.Uniform style.color)
      , SvgA.fontFamily <|
          case style.typeface of
            Text.Serif -> "serif"
            Text.Sansserif -> "sans-serif"
            Text.Monospace -> "monospace"
            Text.Font name -> name
      , SvgA.fontSize <| fromInt style.size
      , SvgA.fontWeight <|
          case style.weight of
            Text.Thin -> "200"
            Text.Light -> "300"
            Text.Regular -> "normal"
            Text.Medium -> "500"
            Text.SemiBold -> "600"
            Text.Bold -> "bold"
            Text.Black -> "800"
      , SvgA.fontStyle <|
          case style.shape of
            Text.Upright -> "normal"
            Text.SmallCaps -> "normal"
            Text.Slanted -> "oblique"
            Text.Italic -> "italic"
      , SvgA.fontVariant <|
          case style.shape of
            Text.SmallCaps -> "small-caps"
            _ -> "normal"
      , SvgA.textDecoration <|
          case style.line of
            Text.None -> "none"
            Text.Under -> "underline"
            Text.Over -> "overline"
            Text.Through -> "line-through"
      , SvgA.textAnchor <| "middle"
      , SvgA.dominantBaseline "middle"
      , SvgA.opacity <| fromFloat collage.opacity
      , SvgA.transform <| decodeTransform collage
      ]
    _ ->
      [ SvgA.opacity <| fromFloat collage.opacity
      , SvgA.transform <| decodeTransform collage
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
