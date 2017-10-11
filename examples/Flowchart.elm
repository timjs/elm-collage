module Flowchart exposing (main)

import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (..)
import Collage.Text as Text exposing (Shape(..), fromString)
import Color exposing (..)
import Html exposing (Html)
import List exposing (head)


-- Data ------------------------------------------------------------------------


type Flow
    = Finish
    | Task String
    | Sequence Flow Flow
    | Choice String (List Flow)
    | Parallel (List Flow)


example : Flow
example =
    Sequence
        (Sequence (Task "check diff")
            (Choice "diff is as whished"
                [ Sequence
                    (Parallel
                        [ Task "prepare changelog"
                        , Task "bump version"
                        ]
                    )
                    (Task "publish")
                , Task "work harder"
                ]
            )
        )
        Finish



-- Elements --------------------------------------------------------------------


unit : Float
unit =
    30


space : Collage msg
space =
    spacer unit unit


thinline : LineStyle
thinline =
    solid thin (uniform black)


thickline : LineStyle
thickline =
    solid ultrathick (uniform black)


diamond : String -> Collage msg
diamond label =
    let
        text =
            fromString label
                |> Text.shape Italic
                |> rendered

        w =
            width text

        l =
            unit / 2

        points =
            [ ( 0, l )
            , ( -l, 0 )
            , ( 0, -l )
            , ( w, -l )
            , ( w + l, 0 )
            , ( w, l )
            ]

        shape =
            polygon points
                |> styled
                    ( uniform lightPurple
                    , thinline
                    )
                |> center
    in
    impose text shape


box : String -> Collage msg
box label =
    let
        text =
            fromString label
                |> rendered

        w =
            width text + l

        l =
            unit

        shape =
            rectangle w l
                |> styled
                    ( uniform lightBlue
                    , thinline
                    )
    in
    impose text shape


dot : Collage msg
dot =
    circle (unit / 3)
        |> styled
            ( uniform green
            , thinline
            )


arrow : Float -> Collage msg
arrow length =
    let
        body =
            line length
                |> traced thinline
                |> rotate (pi / 2)

        tip =
            triangle (unit / 3)
                |> filled (uniform black)
                |> rotate pi
    in
    --FIXME: add markers
    vertical [ body, tip ]



-- Rendering -------------------------------------------------------------------


render : Flow -> Collage msg
render flow =
    let
        addBottomArrow max flow =
            vertical
                [ flow
                , arrow (max - height flow)
                ]

        addBottomLine max flow =
            vertical
                [ flow
                , line (max - height flow)
                    |> traced thinline
                    |> rotate (pi / 2)
                ]

        branches flows =
            let
                prerender =
                    flows
                        |> List.map render

                l =
                    prerender
                        |> group
                        |> height
                        --NOTE: this is the length of a normal arrow
                        |> (+) unit
            in
            prerender
                |> List.map (addBottomArrow l)
                |> List.intersperse space
                |> horizontal
                |> center
    in
    case flow of
        Finish ->
            vertical
                [ arrow unit
                , dot
                ]

        Task string ->
            vertical
                [ arrow unit
                , box string
                ]

        Sequence flow1 flow2 ->
            vertical
                [ render flow1
                , render flow2
                ]

        Choice condition flows ->
            let
                prerendered =
                    flows
                        |> List.map render

                h =
                    prerendered
                        |> group
                        |> height
                        --NOTE: this is the length of a normal arrow
                        |> (+) unit

                inner =
                    prerendered
                        |> List.map (addBottomLine h)
                        |> List.intersperse space
                        |> horizontal
                        |> center
                        |> shift ( -rightMargin, 0 )

                leftMargin =
                    (head prerendered ? empty |> width) / 2

                rightMargin =
                    (last prerendered ? empty |> width) / 2

                bar =
                    line (width inner - leftMargin - rightMargin)
                        |> traced thinline
            in
            vertical
                [ arrow unit
                , vertical
                    [ bar
                    , inner
                    , bar
                    ]
                    |> at top (diamond condition)
                    |> at bottom (diamond "")
                ]

        Parallel flows ->
            let
                inner =
                    branches flows

                length =
                    width inner

                bar =
                    line (length + unit)
                        |> traced thickline
            in
            vertical
                [ arrow unit
                , bar
                , inner
                , bar
                ]



-- Helpers ---------------------------------------------------------------------


last : List a -> Maybe a
last elems =
    case elems of
        [] ->
            Nothing

        [ x ] ->
            Just x

        x :: xs ->
            last xs


(?) =
    flip Maybe.withDefault



-- Main ------------------------------------------------------------------------


main : Html msg
main =
    render example
        |> svg
