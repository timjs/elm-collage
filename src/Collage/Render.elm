module Collage.Render exposing (svg)

{-| Technically, it should be possible to use different backends to render a collage,
but we only provide a Svg backend here.

@docs svg

-}

-- NOTE: Render should only depend on Core, not Collage itself

import Collage.Core as Core exposing (Collage, Point)
import Collage.Layout as Layout
import Collage.Text as Text exposing (Text)
import Color exposing (Color)
import Html exposing (Html)
import List
import String
import Svg exposing (Attribute, Svg)
import Svg.Attributes as Svg
import Svg.Events as Svg
import Tuple exposing (first, second)


{-| Take a collage and render it to Html using Svg.

It uses the automatically calculated envelope from the Collage.Layout module as the viewbox.

-}
svg : Collage msg -> Html msg
svg collage =
    let
        w =
            toString <| Layout.width collage

        h =
            toString <| Layout.height collage
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
                render (Layout.align Layout.topLeft collage) 0
        ]


render : Collage msg -> Int -> ( Int, List (Svg msg) )
render collage id =
    --FIXME: why use ids?
    case collage.basic of
        Core.Path style path ->
            case path of
                Core.Polyline ps ->
                    ( id
                    , [ Svg.polyline
                            ((Svg.points <| decodePoints ps)
                                :: attrs collage id
                                ++ events collage
                            )
                            []
                      ]
                    )

        Core.Shape ( fill, line ) shape ->
            case shape of
                Core.Polygon ps ->
                    ( id + 1
                    , evalFillStyle fill id
                        ++ [ Svg.polygon
                                ((Svg.points <| decodePoints ps)
                                    :: attrs collage id
                                    ++ events collage
                                )
                                []
                           ]
                    )

                Core.Ellipse rx ry ->
                    ( id + 1
                    , evalFillStyle fill id
                        ++ [ Svg.ellipse
                                (attrs collage id
                                    ++ events collage
                                    ++ [ Svg.rx <| toString rx
                                       , Svg.ry <| toString ry
                                       ]
                                )
                                []
                           ]
                    )

                Core.Loop path ->
                    render { collage | basic = Core.Path line path } id

        Core.Text _ (Text.Text style str) ->
            ( id
            , [ Svg.text_ (attrs collage id ++ events collage)
                    [ Svg.text str ]
              ]
            )

        Core.Image ( width, height ) url ->
            ( id
            , [ Svg.image
                    (attrs collage id
                        ++ events collage
                        ++ [ Svg.width <| toString width
                           , Svg.height <| toString height
                           , Svg.xlinkHref url
                           ]
                    )
                    []
              ]
            )

        Core.Element ( width, height ) elem ->
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
                    [ Svg.foreignObject ([ Svg.width w, Svg.height h ] ++ attrs collage id ++ events collage)
                        [ elem ]
                    ]
              ]
            )

        Core.Group forms ->
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
            ( id_, [ Svg.g (attrs collage id ++ events collage) <| forms_ ] )

        Core.Subcollage fore back ->
            --NOTE: rendering a subcollage is the same as rendering a group, only layout calculations in `Collage.Layout` differ.
            --FIXME: as in `Collage`: order in list should be reversed by renderer
            render (Core.collage <| Core.Group [ back, fore ]) id


events : Collage msg -> List (Attribute msg)
events { handlers } =
    List.map (uncurry Svg.on) handlers


attrs : Collage msg -> Int -> List (Attribute msg)
attrs collage id =
    case collage.basic of
        Core.Path line _ ->
            [ Svg.stroke <| decodeFill line.fill id
            , Svg.strokeOpacity <| decodeFillAlpha line.fill
            , Svg.strokeWidth <| toString line.thickness
            , Svg.strokeLinecap <| decodeCap line.cap
            , Svg.strokeLinejoin <| decodeJoin line.join
            , Svg.opacity <| toString collage.alpha
            , Svg.transform <| evalTransform collage
            , Svg.strokeDashoffset <| toString line.dashPhase
            , Svg.strokeDasharray <| decodeDashing line.dashPattern
            ]

        Core.Shape ( fill, line ) _ ->
            [ Svg.fill <| decodeFill fill id
            , Svg.fillOpacity <| decodeFillAlpha fill
            , Svg.stroke <| decodeFill line.fill id
            , Svg.strokeOpacity <| decodeFillAlpha line.fill
            , Svg.strokeWidth <| toString line.thickness
            , Svg.strokeLinecap <| decodeCap line.cap
            , Svg.strokeLinejoin <| decodeJoin line.join
            , Svg.opacity <| toString collage.alpha
            , Svg.transform <| evalTransform collage
            , Svg.strokeDashoffset <| toString line.dashPhase
            , Svg.strokeDasharray <| decodeDashing line.dashPattern
            ]

        Core.Text _ (Text.Text style str) ->
            [ Svg.fill <| decodeFill (Core.Uniform style.color) id
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

                    Text.Normal ->
                        "normal"
            , Svg.fontStyle <|
                case style.shape of
                    Text.Italic ->
                        "italic"

                    Text.Upright ->
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
            , Svg.transform <| evalTransform collage
            ]

        _ ->
            [ Svg.transform <| evalTransform collage ]


decodeCap : Core.LineCap -> String
decodeCap cap =
    case cap of
        Core.Round ->
            "round"

        Core.Padded ->
            "square"

        Core.Flat ->
            "butt"


decodeJoin : Core.LineJoin -> String
decodeJoin join =
    case join of
        Core.Smooth ->
            "round"

        Core.Sharp ->
            "milter"

        Core.Clipped ->
            "bevel"


decodePoints : List Point -> String
decodePoints ps =
    ps |> List.map (\( x, y ) -> String.join "," [ toString x, toString y ]) |> String.join " "


evalTransform : Collage msg -> String
evalTransform object =
    let
        x =
            toString <| first object.origin

        y =
            toString <| second object.origin

        theta =
            toString <| object.theta / 2 / pi * 360

        scale =
            toString object.scale
    in
    String.concat
        [ "translate(", x, ",", y, ") rotate(", theta, ") scale(", scale, ")" ]


evalFillStyle : Core.FillStyle -> Int -> List (Svg msg)
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
                           (\( off, collage ) ->
                               Svg.stop
                                   [ Svg.offset <| toString off
                                   , Svg.stopColor <| decodeColor collage
                                   , Svg.stopOpacity <| decodeAlpha collage
                                   ]
                                   []
                           )
                           stops
                   ]
               ]

        -}
        _ ->
            []


decodeFill : Core.FillStyle -> Int -> String
decodeFill fs id =
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
    ds |> List.map (\( x, y ) -> String.join "," [ toString x, toString y ]) |> String.join " "
