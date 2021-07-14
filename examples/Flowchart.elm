module Flowchart exposing (main)

import Example
import Collage exposing (..)
import Collage.Layout as Layout exposing (..)
import Collage.Text as Text exposing (Shape(..), fromString, Typeface(..))
import Color exposing (..)
import Html exposing (Html)
import Html.Attributes
import List exposing (head)



-- Data ------------------------------------------------------------------------


type Flow
    = Finish
    | Task String
    | Sequence Flow Flow
    | Choice String Flow Flow
    | Parallel (List Flow)


example : Flow
example =
    Sequence
        (Sequence (Task "check diff")
            (Choice "diff is as wished"
                (Sequence
                    (Parallel
                        [ Task "prepare changelog"
                        , Task "bump version"
                        ]
                    )
                    (Task "publish")
                )
                (Task "work harder")
            )
        )
        Finish



-- Elements --------------------------------------------------------------------


fontFamily : String
fontFamily =
    "Caveat"


font : Typeface
font =
    Font fontFamily


unit : Float
unit =
    50


space : Collage msg
space =
    spacer unit unit


thinline : LineStyle
thinline =
    { defaultLineStyle
        | thickness = verythin
        , cap = Padded
    }


thickline : LineStyle
thickline =
    { defaultLineStyle
        | thickness = semithick
        , cap = Padded
    }


diamond : String -> Collage msg
diamond label =
    let
        text =
            fromString label
                |> Text.shape Italic
                |> Text.size Text.huge
                |> Text.weight Text.Black
                |> Text.typeface font
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
                |> name ("diamond" ++ label)
    in
    impose text shape


box : String -> Collage msg
box label =
    let
        text =
            fromString label
                |> Text.typeface (Text.Font fontFamily)
                |> Text.size Text.huge
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
    circle (unit)
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
        addBottomArrow max flow_ =
            vertical
                [ flow_
                , arrow (max - height flow_)
                ]

        addBottomLine max flow_ =
            vertical
                [ flow_
                , line (max - height flow_)
                    |> traced thinline
                    |> rotate (pi / 2)
                ]

        branches finishing flows =
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
            in
            prerendered
                |> List.map (finishing h)
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

        Choice condition left right ->
            let
                ( leftBranch, rightBranch ) =
                    ( render left, render right )

                maxHeight =
                    max (height leftBranch) (height rightBranch) + unit

                inner =
                    horizontal
                        [ leftBranch
                            |> addBottomLine maxHeight
                            |> name "leftBranch"
                        , space
                        , rightBranch
                            |> addBottomLine maxHeight
                            |> name "rightBranch"
                        ]
                        |> shift ( -(envelope Right leftBranch + unit + envelope Left rightBranch) / 2, 0 )
            in
            vertical
                [ arrow unit
                , inner
                    |> at top (diamond condition)
                    |> at bottom (diamond "")
                    |> connect [ ( "leftBranch", top ), ( "diamonddiff is as wished", Layout.left ) ] thinline
                    |> connect [ ( "diamonddiff is as wished", Layout.right ), ( "rightBranch", Layout.top ) ] thinline
                    |> connect [ ( "leftBranch", bottom ), ( "diamond", Layout.left ) ] thinline
                    |> connect [ ( "diamond", Layout.right ), ( "rightBranch", Layout.bottom ) ] thinline
                ]

        Parallel flows ->
            let
                inner =
                    branches addBottomArrow flows

                bar =
                    line (width inner + unit)
                        |> traced thickline
            in
            vertical
                [ arrow unit
                , bar
                , inner
                , bar
                ]



-- Main ------------------------------------------------------------------------


type Msg
    = NoOp


type alias Model =
    Flow


main : Platform.Program () (Example.Model Msg Model) (Example.Msg Msg)
main =
    Example.example
        { init = example
        , update = (\_ model -> model)
        , render = render
        , view = view
        }


view : Html Msg -> Html Msg
view collage =
    Html.div []
        [ Html.node "link" [ Html.Attributes.href ("https://fonts.googleapis.com/css2?family=" ++ fontFamily ++ "&display=swap"), Html.Attributes.rel "stylesheet" ]  []
        , collage
        ]
