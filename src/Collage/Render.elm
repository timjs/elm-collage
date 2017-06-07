module Collage.Render exposing (svg)

{-| TODO

@docs svg

-}

-- NOTE: Render should only depend on Core, not Collage itself

import Collage.Core exposing (..)
import Collage.Layout as Layout
import Color exposing (Color)
import Html exposing (Html)
import List
import String
import Svg exposing (Attribute, Svg)
import Svg.Attributes as Svg exposing (style)
import Svg.Events as Svg
import Text exposing (Text)
import Tuple exposing (first, second)


{-| Takes a `Collage` and renders it to usable HTML, in this case
in the col of an SVG element. The first two arguments determine
the height and width of the SVG viewbox in pixels.
-}
svg : Collage msg -> Html msg
svg col =
    let
        w =
            toString <| Layout.width col

        h =
            toString <| Layout.height col
    in
    Html.div
        []
        [ Svg.svg
            [ Svg.width w
            , Svg.height h
            , Svg.version "1.1"
            ]
          <|
            second <|
                render (Layout.northwest col) 0
        ]


render : Collage msg -> Int -> ( Int, List (Svg msg) )
render col id =
    --FIXME: why use ids?
    case col.basic of
        Path style path ->
            case path of
                Polyline ps ->
                    ( id
                    , [ Svg.polyline
                            ((Svg.points <| decodePoints ps)
                                :: attrs col id
                                ++ events col
                            )
                            []
                      ]
                    )

        Shape ( fill, line ) shape ->
            case shape of
                Polygon ps ->
                    ( id + 1
                    , evalFillStyle fill id
                        ++ [ Svg.polygon
                                ((Svg.points <| decodePoints ps)
                                    :: attrs col id
                                    ++ events col
                                )
                                []
                           ]
                    )

                Ellipse rx ry ->
                    ( id + 1
                    , evalFillStyle fill id
                        ++ [ Svg.ellipse
                                (attrs col id
                                    ++ events col
                                    ++ [ Svg.rx <| toString rx
                                       , Svg.ry <| toString ry
                                       ]
                                )
                                []
                           ]
                    )

        Text (Text.Text style str) ->
            ( id
            , [ Svg.text_ (attrs col id ++ events col)
                    [ Svg.text str ]
              ]
            )

        Image width height url ->
            ( id
            , [ Svg.image
                    (attrs col id
                        ++ events col
                        ++ [ Svg.width <| toString width
                           , Svg.height <| toString height
                           , Svg.xlinkHref url
                           ]
                    )
                    []
              ]
            )

        Element width height elem ->
            let
                tx =
                    toString <| -(width / 2)

                ty =
                    toString <| -(height / 2)

                w =
                    toString width

                h =
                    toString height
            in
            ( id
            , [ Svg.g [ Svg.transform <| String.concat [ "translate(", tx, ",", ty, ")" ] ]
                    [ Svg.foreignObject ([ Svg.width w, Svg.height h ] ++ attrs col id ++ events col)
                        [ elem ]
                    ]
              ]
            )

        Group forms ->
            let
                go ( i, rs ) fs =
                    case fs of
                        [] ->
                            ( i, rs )

                        x :: xs ->
                            let
                                ( i_, rs_ ) =
                                    render x i
                            in
                            go ( i + i_, rs ++ rs_ ) xs

                ( id_, forms_ ) =
                    go ( id, [] ) forms
            in
            ( id_, [ Svg.g (attrs col id ++ events col) <| forms_ ] )


events : Collage msg -> List (Attribute msg)
events { handlers } =
    List.map (uncurry Svg.on) handlers


attrs : Collage msg -> Int -> List (Attribute msg)
attrs col id =
    case col.basic of
        Path style _ ->
            [ Svg.stroke <| decodeFill style.fill id
            , Svg.strokeOpacity <| decodeFillAlpha style.fill
            , Svg.strokeWidth <| toString style.thickness
            , Svg.strokeLinecap <| decodeCap style.cap
            , Svg.strokeLinejoin <| decodeJoin style.join
            , Svg.opacity <| toString col.alpha
            , Svg.transform <| evalTransform col
            , Svg.strokeDashoffset <| toString style.dashPhase
            , Svg.strokeDasharray <| decodeDashing style.dashPattern
            ]

        Shape ( fill, line ) _ ->
            [ Svg.fill <| decodeFill fill id
            , Svg.fillOpacity <| decodeFillAlpha fill
            , Svg.stroke <| decodeFill line.fill id
            , Svg.strokeOpacity <| decodeFillAlpha line.fill
            , Svg.strokeWidth <| toString line.thickness
            , Svg.strokeLinecap <| decodeCap line.cap
            , Svg.strokeLinejoin <| decodeJoin line.join
            , Svg.opacity <| toString col.alpha
            , Svg.transform <| evalTransform col
            , Svg.strokeDashoffset <| toString line.dashPhase
            , Svg.strokeDasharray <| decodeDashing line.dashPattern
            ]

        Text (Text.Text style str) ->
            [ Svg.fill <| decodeFill (Uniform style.color) id
            , Svg.fontFamily <|
                case style.face of
                    Text.Roman ->
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
                    Text.Bold ->
                        "bold"

                    --FIXME: add more
                    _ ->
                        "normal"
            , Svg.fontStyle <|
                case style.shape of
                    Text.Italic ->
                        "italic"

                    --FIXME: add more
                    _ ->
                        "normal"
            , Svg.textDecoration <|
                case style.line of
                    Just Text.Under ->
                        "underline"

                    --FIXME: add more
                    _ ->
                        "none"
            , Svg.textAnchor <| "middle"
            , Svg.dominantBaseline "middle"
            , Svg.transform <| evalTransform col
            ]

        _ ->
            [ Svg.transform <| evalTransform col ]


decodeCap : LineCap -> String
decodeCap cap =
    case cap of
        Round ->
            "round"

        Padded ->
            "square"

        Flat ->
            "butt"


decodeJoin : LineJoin -> String
decodeJoin join =
    case join of
        Smooth ->
            "round"

        Sharp ->
            "milter"

        Clipped ->
            "bevel"


decodePoints : List Point -> String
decodePoints ps =
    ps |> List.map (\( x, y ) -> [ toString x, toString y ]) |> List.concat |> String.join " "


evalTransform : Collage msg -> String
evalTransform obj =
    let
        x =
            toString <| first obj.origin

        y =
            toString <| second obj.origin

        theta =
            toString <| obj.theta / 2 / pi * 360

        scale =
            toString obj.scale
    in
    String.concat
        [ "translate(", x, ",", y, ") rotate(", theta, ") scale(", scale, ")" ]


evalFillStyle : FillStyle -> Int -> List (Svg msg)
evalFillStyle fs id =
    --FIXME: change name
    case fs of
        {- Pattern w h url a ->
               [ Svg.defs []
                   [ Svg.pattern
                       [ Svg.width <| toString w
                       , Svg.height <| toString h
                       , Svg.patternUnits "userSpaceOnUse"
                       , Svg.id <| "UUID" ++ toString id
                       ]
                       [ Svg.image
                           [ Svg.width <| toString w
                           , Svg.height <| toString h
                           , Svg.xlinkHref url
                           ]
                           []
                       ]
                   ]
               ]

           Linear theta stops ->
               [ Svg.defs []
                   [ Svg.linearGradient
                       [ Svg.id <| "UUID" ++ toString id
                       , Svg.gradientTransform <|
                           "rotate("
                               ++ toString (theta / 2 / pi * 360)
                               ++ ")"
                       ]
                     <|
                       List.map
                           (\( off, col ) ->
                               Svg.stop
                                   [ Svg.offset <| toString off
                                   , Svg.stopColor <| decodeColor col
                                   , Svg.stopOpacity <| decodeAlpha col
                                   ]
                                   []
                           )
                           stops
                   ]
               ]

        -}
        _ ->
            []


decodeFill : FillStyle -> Int -> String
decodeFill fs id =
    case fs of
        Uniform c ->
            decodeColor c

        Transparent ->
            "none"


decodeFillAlpha : FillStyle -> String
decodeFillAlpha fs =
    case fs of
        Uniform c ->
            decodeAlpha c

        Transparent ->
            "0"



{- Pattern _ _ _ a ->
   toString a
-}


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
            c |> Color.toRgb
    in
    toString alpha


decodeDashing : List ( Int, Int ) -> String
decodeDashing ds =
    ds |> List.map (\( x, y ) -> String.concat [ toString x, ",", toString y ]) |> String.join ","
