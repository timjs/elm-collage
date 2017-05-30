module Collage.Svg exposing (svg)

{-| TODO

@docs svg

-}

import Html exposing (Html)
import Svg exposing (Svg, Attribute)
import Svg.Attributes as Svg exposing (style)
import Svg.Events as Svg
import String
import Tuple exposing (first, second)
import Color exposing (Color)
import List
import Collage exposing (Point, Form, BasicForm(..), Path(..), Shape(..), Texture(..), Stroke, LineCap(..), LineJoin(..))


{-| Takes a `Form` and renders it to usable HTML, in this case
in the form of an SVG element. The first two arguments determine
the height and width of the SVG viewbox in pixels.
-}
svg : Float -> Float -> Form msg -> Html msg
svg width height form =
    Html.div
        []
        [ Svg.svg
            [ Svg.width <| toString width
            , Svg.height <| toString height
            , Svg.version "1.1"
            ]
          <|
            second <|
                render form 0
        ]


render : Form msg -> Int -> ( Int, List (Svg msg) )
render form id =
    case form.form of
        Path line style ->
            case line of
                Polyline ps ->
                    ( id
                    , [ Svg.polyline
                            ((Svg.points <| decodePoints ps)
                                :: attrs form id
                                ++ events form
                            )
                            []
                      ]
                    )

        Shape shape style ->
            case shape of
                Polygon ps ->
                    ( id + 1
                    , evalTexture style.fill id
                        ++ [ Svg.polygon
                                ((Svg.points <| decodePoints ps)
                                    :: attrs form id
                                    ++ events form
                                )
                                []
                           ]
                    )

                Ellipse rx ry ->
                    ( id + 1
                    , evalTexture style.fill id
                        ++ [ Svg.ellipse
                                (attrs form id
                                    ++ events form
                                    ++ [ Svg.rx <| toString rx
                                       , Svg.ry <| toString ry
                                       ]
                                )
                                []
                           ]
                    )

        {- Text text align ->
           case text of
               Text t style ->
                   ( id
                   , [ Svg.text_ (attrs form id ++ events form)
                           [ Svg.text t ]
                     ]
                   )
        -}
        Image url width height ->
            ( id
            , [ Svg.image
                    (attrs form id
                        ++ events form
                        ++ [ Svg.width <| toString width
                           , Svg.height <| toString height
                           , Svg.xlinkHref url
                           ]
                    )
                    []
              ]
            )

        Element elem ->
            ( id
            , [ Svg.foreignObject (attrs form id ++ events form)
                    [ elem ]
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
                ( id_, [ Svg.g (attrs form id ++ events form) <| forms_ ] )


events : Form msg -> List (Attribute msg)
events { handlers } =
    List.map (uncurry Svg.on) handlers


attrs : Form msg -> Int -> List (Attribute msg)
attrs form id =
    case form.form of
        Path line style ->
            [ Svg.stroke <| decodeTexture style.texture id
            , Svg.strokeOpacity <| decodeTextureAlpha style.texture
            , Svg.strokeWidth <| toString style.thickness
            , Svg.strokeLinecap <| decodeCap style.cap
            , Svg.strokeLinejoin <| decodeJoin style.join
            , Svg.opacity <| toString form.alpha
            , Svg.transform <| evalTransform form
            , Svg.strokeDashoffset <| toString style.dashOffset
            , Svg.strokeDasharray <| decodeDashing style.dashing
            ]

        Shape shape style ->
            [ Svg.fill <| decodeTexture style.fill id
            , Svg.fillOpacity <| decodeTextureAlpha style.fill
            , Svg.stroke <| decodeTexture style.line.texture id
            , Svg.strokeOpacity <| decodeTextureAlpha style.line.texture
            , Svg.strokeWidth <| toString style.line.thickness
            , Svg.strokeLinecap <| decodeCap style.line.cap
            , Svg.strokeLinejoin <| decodeJoin style.line.join
            , Svg.opacity <| toString form.alpha
            , Svg.transform <| evalTransform form
            , Svg.strokeDashoffset <| toString style.line.dashOffset
            , Svg.strokeDasharray <| decodeDashing style.line.dashing
            ]

        {- Text text align ->
           let
               style =
                   case text of
                       Text _ s ->
                           s

               align_ =
                   case align of
                       Center ->
                           "middle"

                       Left ->
                           "start"

                       Right ->
                           "end"
           in
               [ Svg.fill <| decodeTexture style.stroke id
               , Svg.fontFamily style.font
               , Svg.fontSize <| toString style.size
               , Svg.fontWeight <|
                   if style.bold then
                       "bold"
                   else
                       "normal"
               , Svg.fontStyle <|
                   if style.italic then
                       "oblique"
                   else
                       "normal"
               , Svg.textDecoration <|
                   if style.underlined then
                       "underline"
                   else
                       "none"
               , Svg.textAnchor <| align_
               , Svg.dominantBaseline "middle"
               , Svg.evalTransform <| evalTransform form
               ]

        -}
        _ ->
            [ Svg.transform <| evalTransform form ]


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


evalTransform :
    { r
        | x : Float
        , y : Float
        , theta : Float
        , scale : Float
    }
    -> String
evalTransform obj =
    let
        x =
            toString <| obj.x

        y =
            toString <| obj.y

        theta =
            toString <| obj.theta / 2 / pi * 360

        scale =
            toString obj.scale
    in
        String.concat
            [ "translate(", x, ",", y, ") rotate(", theta, ") scale(", scale, ")" ]


evalTexture : Texture -> Int -> List (Svg msg)
evalTexture fs id =
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


decodeTexture : Texture -> Int -> String
decodeTexture fs id =
    case fs of
        Uniform c ->
            decodeColor c

        Transparent ->
            "none"


decodeTextureAlpha : Texture -> String
decodeTextureAlpha fs =
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
