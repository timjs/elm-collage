module Collage.Layout
    exposing
        ( Direction
        , above
        , after
        , base
        , before
        , below
        , beside
        , east
        , empty
        , envelope
        , height
        , horizontal
        , north
        , northeast
        , northwest
        , place
        , showEnvelope
        , showOrigin
        , south
        , southeast
        , southwest
        , spacer
        , stack
        , vertical
        , west
        , width
        )

{-| TODO

@docs Direction, envelope, width, height
@docs spacer, empty
@docs stack, place, beside, before, after, above, below, horizontal, vertical
@docs north, northeast, east, southeast, south, southwest, west, northwest, base
@docs showOrigin, showEnvelope

-}

import Collage exposing (..)
import Collage.Core as Core
import Color
import Maybe.Extra exposing ((?))
import Tuple exposing (first, second)


-- Directions ------------------------------------------------------------------


{-| Represents a `flow` direction for a list of elements.
-}
type Direction
    = Up
    | Down
    | Right
    | Left


{-| -}
opposite : Direction -> Direction
opposite dir =
    case dir of
        Up ->
            Down

        Down ->
            Up

        Right ->
            Left

        Left ->
            Right



-- Envelopes -------------------------------------------------------------------


{-| -}
envelope : Direction -> Collage msg -> Float
envelope dir col =
    let
        env =
            handleBasic dir col.theta col.basic

        ( tx, ty ) =
            col.origin
    in
    col.scale
        * (case dir of
            Up ->
                max 0 (env - ty)

            Down ->
                max 0 (env + ty)

            Right ->
                max 0 (env + tx)

            Left ->
                max 0 (env - tx)
          )


handleBasic : Direction -> Float -> BasicCollage msg -> Float
handleBasic dir theta basic =
    let
        rotate ( x, y ) =
            let
                c =
                    cos theta

                s =
                    sin theta
            in
            ( c * x - s * y, s * x + c * y )

        thicken t ( x, y ) =
            ( if x < 0 then
                x - t / 2
              else
                x + t / 2
            , if y < 0 then
                y - t / 2
              else
                y + t / 2
            )
    in
    case basic of
        Core.Shape ( fill, line ) (Core.Ellipse rx ry) ->
            handleBox dir (rotate ( 2 * rx + line.thickness, 2 * ry + line.thickness ))

        Core.Shape ( _, line ) (Core.Polygon ps) ->
            handlePath dir (List.map (thicken line.thickness << rotate) ps)

        Core.Path line (Core.Polyline ps) ->
            handlePath dir (List.map (thicken line.thickness << rotate) ps)

        Core.Text text ->
            --FIXME: calculate envelope for Text
            0

        Core.Image width height _ ->
            handleBox dir (rotate ( width, height ))

        Core.Element width height _ ->
            handleBox dir (rotate ( width, height ))

        Core.Group forms ->
            --FIXME: correct with translation???
            (List.maximum <| List.map (envelope dir) forms) ? 0


handlePath : Direction -> List Point -> Float
handlePath dir ps =
    let
        xs =
            List.map first ps

        ys =
            List.map second ps
    in
    case dir of
        -- NOTE: be aware of the switched vertical coordinate system of Svg
        Up ->
            -(List.minimum ys ? 0)

        -- NOTE: be aware of the switched vertical coordinate system of Svg
        Down ->
            List.maximum ys ? 0

        Right ->
            List.maximum xs ? 0

        Left ->
            -(List.minimum xs ? 0)


handleBox : Direction -> ( Float, Float ) -> Float
handleBox dir ( width, height ) =
    case dir of
        Up ->
            height / 2

        Down ->
            height / 2

        Right ->
            width / 2

        Left ->
            width / 2



-- Layouts ---------------------------------------------------------------------
{-
   These functions are subject to change! Practise will show if they work well in
   production code. These kind of definitions are always confusing...
-}


{-| Create an empty Collage of given width and height.

This is useful for getting your spacing right and for making borders.

-}
spacer : Float -> Float -> Collage msg
spacer w h =
    rectangle w h |> styled ( transparent, invisible )


{-| An Element that takes up no space. Good for things that appear conditionally:

    horizontal [ img1, if showMore then img2 else empty ]

  - Note: this is the identity element of the monoid on Collages.

-}
empty : Collage msg
empty =
    spacer 0 0


{-| Place a list of diagrams on top of each other,
with their origin points stacked on the "out of page" axis.
The first Collage in the list is on top.
This is the same as the `group` operation in the Collage module.

    stack [a, b, c]

        +---+
        | a |
        +---+

  - Note: this is called `concat` in Diagrams.

FIXME:

    stack a b -- read: "stack a on b"

  - Note: this is the binary operation of the Semigroup of Collages.
  - Note: this is called `(<>)` or `atop` in Diagrams.

-}
stack : Collage msg -> Collage msg -> Collage msg
stack a b =
    Collage.group [ a, b ]


{-|

  - Note: called `juxtapose` in Diagrams

-}
place : Direction -> Collage msg -> Collage msg -> Collage msg
place dir a b =
    let
        len =
            envelope dir a + envelope (opposite dir) b

        move =
            case dir of
                Up ->
                    -- NOTE: translating means **minus** because of switched vertical axis
                    ( 0, -len )

                Down ->
                    -- NOTE: translating means **plus** because of switched vertical axis
                    ( 0, len )

                Right ->
                    ( len, 0 )

                Left ->
                    ( -len, 0 )
    in
    shift move b


{-|

  - Note: same as `beside` in Diagrams

-}
beside : Direction -> Collage msg -> Collage msg -> Collage msg
beside dir a b =
    stack a (place dir a b)


{-| Given two diagrams a and b, place b to the **left** of a,
such that their origins are on a horizontal line and their envelopes touch.
The origin of the new Collage is the origin of a. FIXME: true?
Sumarised:

    before a b -- read: before a, put b

Which is equivallent to:

    b
        |> before a

  - Warning: Do net read this infix, arguments are swapped with regard to Diagrams!

-}
before : Collage msg -> Collage msg -> Collage msg
before =
    beside Left


{-| Given two diagrams a and b, place b to the **right** of a,
such that their origins are on a horizontal line and their envelopes touch.
The origin of the new Collage is the origin of a. FIXME: true?
Sumarised:

    after a b -- read: after a, put b

  - Warning: The `(|>)` doesn't read well combined with `after`, don't use it!
  - Warning: Do net read this infix, arguments are swapped with regard to Diagrams!

-}
after : Collage msg -> Collage msg -> Collage msg
after =
    beside Right


{-| Given two forms a and b, place b **above** a,
such that their origins are on a vertical line and their envelopes touch.
The origin of the new Collage is the base of a and b. FIXME: true?
Summarised:

    above a b -- read: above a, put b

Which is equivallent to

    b
        |> above a

  - Warning: Do net read this infix, arguments are swapped with regard to Diagrams!

-}
above : Collage msg -> Collage msg -> Collage msg
above =
    beside Up


{-| Given two forms a and b, place b **below** a,
such that their origins are on a vertical line and their envelopes touch.
The origin of the new Collage is the base of a and b. FIXME: true?
Summarised:

    below a b -- read: below a, put b

  - Warning: The `(|>)` doesn't read well combined with `below`, don't use it!
  - Warning: Do net read this infix, arguments are swapped with regard to Diagrams!

-}
below : Collage msg -> Collage msg -> Collage msg
below =
    beside Down


{-| Place a list of Collages next to each other,
such that their origins are along a horizontal line.
The first element in the list will be on the left, the last on the right.

    horizontal [a, b, c]

       +---+---+---+
       | a | b | c |
       +---+---+---+

  - Note: this is called `hcat` in Diagrams

-}
horizontal : List (Collage msg) -> Collage msg
horizontal =
    List.foldr after empty


{-| Place a list of Diagrams next to each other,
such that their origins are along a vertical line.
The first element in the list will be on the top, the last on the bottom.

    vertical [a, b, c]

       +---+
       | a |
       +---+
       | b |
       +---+
       | c |
       +---+

  - Note: this is called `vcat` in Diagrams

-}
vertical : List (Collage msg) -> Collage msg
vertical =
    List.foldr below empty



-- Queries ---------------------------------------------------------------------


{-| -}
width : Collage msg -> Float
width col =
    envelope Left col + envelope Right col


{-| -}
height : Collage msg -> Float
height col =
    envelope Up col + envelope Down col



-- Anchors ---------------------------------------------------------------------
--TODO: rename to `align top/right/bottom/left` where `align : Anchor -> Collage msg -> Collage msg`?


{-| Shift a Collage such that the origin is on the top edge of the bounding box.
-}
north : Collage msg -> Collage msg
north col =
    shift ( 0, envelope Up col ) col


{-| -}
northeast : Collage msg -> Collage msg
northeast =
    north << east


{-| Shift a Collage such that the origin is on the right edge of the bounding box.
-}
east : Collage msg -> Collage msg
east col =
    shift ( -(envelope Right col), 0 ) col


{-| -}
southeast : Collage msg -> Collage msg
southeast =
    south << east


{-| Shift a Collage such that the origin is on the bottom edge of the bounding box.
-}
south : Collage msg -> Collage msg
south col =
    shift ( 0, -(envelope Down col) ) col


{-| -}
southwest : Collage msg -> Collage msg
southwest =
    south << west


{-| Shift a Collage such that the origin is on the left edge of the bounding box.
-}
west : Collage msg -> Collage msg
west col =
    shift ( envelope Left col, 0 ) col


{-| -}
northwest : Collage msg -> Collage msg
northwest =
    north << west


{-| Shift a Collage such that the envelope in all directions is equal.

  - Note: The anchor of every Collage defaults to this.
    Only use this function to "correct" previous translations of the origin.

-}
base : Collage msg -> Collage msg
base col =
    let
        left =
            envelope Left col

        right =
            envelope Right col

        tx =
            (right - left) / 2

        up =
            envelope Up col

        down =
            envelope Down col

        ty =
            (down - up) / 2
    in
    shift ( -tx, ty ) col



-- Debuging --------------------------------------------------------------------


{-| Draw a red dot at `(0, 0)` in the diagram's local vector space.
-}
showOrigin : Collage msg -> Collage msg
showOrigin col =
    let
        origin =
            circle 3
                |> filled (uniform Color.red)
    in
    stack origin col


{-| Draw a red dot box around a diagram.
-}
showEnvelope : Collage msg -> Collage msg
showEnvelope col =
    --FIXME: add
    -- OR
    --  |> shift col.origin
    -- after stacking to fix frame drawing
    let
        outline =
            rectangle (width col) (height col)
                |> outlined (dot 2 (uniform Color.red))
                |> shift col.origin
    in
    stack outline col
