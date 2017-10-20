module Collage.Render exposing (svg, svgBox)

{-| Technically, it should be possible to use different backends to render a collage,
but we only provide a Svg backend here.

@docs svg, svgBox

-}

import Collage exposing (Collage, Point)
import Collage.Core as Core
import Collage.Layout as Layout
import Collage.Text as Text exposing (Text)
import Color exposing (Color)
import Html exposing (Html)
import List
import String
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


svgAbsolute : ( Float, Float ) -> Collage msg -> Html msg
svgAbsolute ( width, height ) collage =
    let
        w =
            toString width

        h =
            toString height
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
    case collage.basic of
        Core.Path style path ->
            case path of
                Core.Polyline ps ->
                    Svg.polyline
                        ((Svg.points <| decodePoints ps)
                            :: attrs collage
                            ++ events collage
                        )
                        []

        Core.Shape ( fill, line ) shape ->
            case shape of
                Core.Polygon ps ->
                    Svg.polygon
                        ((Svg.points <| decodePoints ps)
                            :: attrs collage
                            ++ events collage
                        )
                        []

                Core.Circle r ->
                    Svg.circle
                        ((Svg.r <| toString r)
                            :: attrs collage
                            ++ events collage
                        )
                        []

                Core.Ellipse rx ry ->
                    Svg.ellipse
                        ([ Svg.rx <| toString rx
                         , Svg.ry <| toString ry
                         ]
                            ++ attrs collage
                            ++ events collage
                        )
                        []

                Core.Rectangle w h r ->
                    Svg.rect
                        ([ Svg.rx <| toString r
                         , Svg.ry <| toString r
                         ]
                            ++ box w h
                            ++ attrs collage
                            ++ events collage
                        )
                        []

                Core.Loop path ->
                    --NOTE: Use the same rendering as for a path
                    render { collage | basic = Core.Path line path }

        Core.Text _ (Core.Chunk style str) ->
            Svg.text_ (attrs collage ++ events collage)
                [ Svg.text str ]

        Core.Image ( w, h ) url ->
            Svg.image
                (Svg.xlinkHref url
                    :: box w h
                    ++ attrs collage
                    ++ events collage
                )
                []

        Core.Html ( w, h ) html ->
            Svg.foreignObject
                (box w h
                    ++ attrs collage
                    ++ events collage
                )
                [ html ]

        Core.Group collages ->
            --NOTE: Order of collages is reversed here!
            Svg.g (attrs collage ++ events collage) <|
                List.foldl (\col res -> render col :: res) [] collages

        Core.Subcollage fore back ->
            --NOTE: Rendering a subcollage is the same as rendering a group, only layout calculations in `Collage.Layout` differ.
            render { collage | basic = Core.Group [ fore, back ] }


box : Float -> Float -> List (Attribute msg)
box w h =
    [ Svg.width <| toString w
    , Svg.height <| toString h
    , Svg.x <| toString (-w / 2)
    , Svg.y <| toString (-h / 2)
    ]


events : Collage msg -> List (Attribute msg)
events { handlers } =
    List.map (uncurry Svg.on) handlers


attrs : Collage msg -> List (Attribute msg)
attrs collage =
    case collage.basic of
        Core.Path line _ ->
            [ Svg.stroke <| decodeFill line.fill
            , Svg.strokeOpacity <| decodeFillAlpha line.fill
            , Svg.strokeWidth <| toString line.thickness
            , Svg.strokeLinecap <| decodeCap line.cap
            , Svg.strokeLinejoin <| decodeJoin line.join
            , Svg.fill <| "none"
            , Svg.opacity <| toString collage.alpha
            , Svg.transform <| evalTransform collage
            , Svg.strokeDashoffset <| toString line.dashPhase
            , Svg.strokeDasharray <| decodeDashing line.dashPattern
            ]

        Core.Shape ( fill, line ) _ ->
            [ Svg.fill <| decodeFill fill
            , Svg.fillOpacity <| decodeFillAlpha fill
            , Svg.stroke <| decodeFill line.fill
            , Svg.strokeOpacity <| decodeFillAlpha line.fill
            , Svg.strokeWidth <| toString line.thickness
            , Svg.strokeLinecap <| decodeCap line.cap
            , Svg.strokeLinejoin <| decodeJoin line.join
            , Svg.opacity <| toString collage.alpha
            , Svg.transform <| evalTransform collage
            , Svg.strokeDashoffset <| toString line.dashPhase
            , Svg.strokeDasharray <| decodeDashing line.dashPattern
            ]

        Core.Text _ (Core.Chunk style str) ->
            [ Svg.fill <| decodeFill (Core.Uniform style.color)
            , Svg.fontFamily <|
                case style.typeface of
                    Text.Serif ->
                        "serif"

                    Text.Sansserif ->
                        "sans-serif"

                    Text.Monospace ->
                        "monospace"

                    Text.Font name ->
                        name
            , Svg.fontSize <| toString style.size
            , Svg.fontWeight <|
                case style.weight of
                    Text.Thin ->
                        "200"

                    Text.Light ->
                        "300"

                    Text.Regular ->
                        "normal"

                    Text.Medium ->
                        "500"

                    Text.SemiBold ->
                        "600"

                    Text.Bold ->
                        "bold"

                    Text.Black ->
                        "800"
            , Svg.fontStyle <|
                case style.shape of
                    Text.Upright ->
                        "normal"

                    Text.SmallCaps ->
                        "normal"

                    Text.Slanted ->
                        "oblique"

                    Text.Italic ->
                        "italic"
            , Svg.fontVariant <|
                case style.shape of
                    Text.SmallCaps ->
                        "small-caps"

                    _ ->
                        "normal"
            , Svg.textDecoration <|
                case style.line of
                    Text.None ->
                        "none"

                    Text.Under ->
                        "underline"

                    Text.Over ->
                        "overline"

                    Text.Through ->
                        "line-through"
            , Svg.textAnchor <| "middle"
            , Svg.dominantBaseline "middle"
            , Svg.transform <| evalTransform collage
            ]

        _ ->
            [ Svg.transform <| evalTransform collage ]


decodeCap : Collage.LineCap -> String
decodeCap cap =
    case cap of
        Collage.Round ->
            "round"

        Collage.Padded ->
            "square"

        Collage.Flat ->
            "butt"


decodeJoin : Collage.LineJoin -> String
decodeJoin join =
    case join of
        Collage.Smooth ->
            "round"

        Collage.Sharp ->
            "miter"

        Collage.Clipped ->
            "bevel"


decodePoints : List Point -> String
decodePoints ps =
    ps |> List.map (\( x, y ) -> String.join "," [ toString x, toString -y ]) |> String.join " "


evalTransform : Collage msg -> String
evalTransform collage =
    let
        x =
            toString <| Tuple.first collage.origin

        y =
            toString <| -(Tuple.second collage.origin)

        theta =
            toString <| -collage.theta / 2 / pi * 360

        sx =
            toString <| Tuple.first collage.scale

        sy =
            toString <| Tuple.second collage.scale
    in
    String.concat
        [ "translate(", x, ",", y, ") rotate(", theta, ") scale(", sx, ",", sy, ")" ]


decodeFill : Core.FillStyle -> String
decodeFill fs =
    case fs of
        Core.Uniform c ->
            decodeColor c

        Core.Transparent ->
            "none"


decodeFillAlpha : Core.FillStyle -> String
decodeFillAlpha fs =
    case fs of
        Core.Uniform c ->
            decodeAlpha c

        Core.Transparent ->
            "0"


decodeColor : Color -> String
decodeColor c =
    let
        { red, green, blue } =
            Color.toRgb c

        r =
            toString red

        g =
            toString green

        b =
            toString blue
    in
    String.concat [ "rgb(", r, ",", g, ",", b, ")" ]


decodeAlpha : Color -> String
decodeAlpha c =
    let
        { alpha } =
            Color.toRgb c
    in
    toString alpha


decodeDashing : List ( Int, Int ) -> String
decodeDashing ds =
    let
        decodeOnOff ( x, y ) =
            String.join "," [ toString x, toString y ]
    in
    ds
        |> List.map decodeOnOff
        |> String.join " "
