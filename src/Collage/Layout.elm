module Collage.Layout
    exposing
        ( Direction(..)
        , envelope
        , spacer
        , empty
        , before
        , after
        , above
        , below
        , horizontal
        , vertical
        , stack
        , north
        , northeast
        , east
        , southeast
        , south
        , southwest
        , west
        , northwest
        , center
        , showOrigin
        , showEnvelope
        )

{-| TODO

@docs Direction, envelope

@docs spacer, empty
@docs before, after, above, below, horizontal, vertical, stack
@docs north, northeast, east, southeast, south, southwest, west, northwest, center
@docs showOrigin, showEnvelope

-}

import Tuple exposing (first, second)
import Maybe.Extra exposing ((?))
import Color
import Collage exposing (..)
import Collage.Core as Core


-- Directions ------------------------------------------------------------------


{-| Represents a `flow` direction for a list of elements.
-}
type Direction
    = Up
    | Down
    | Right
    | Left



-- Envelopes -------------------------------------------------------------------


{-| -}
envelope : Direction -> Collage msg -> Float
envelope dir form =
    let
        env =
            basicEnvelope dir form.basic

        ( tx, ty ) =
            form.origin
    in
        --TODO: rotation
        form.scale
            * case dir of
                Up ->
                    max 0 (env - ty)

                Down ->
                    max 0 (env + ty)

                Right ->
                    max 0 (env + tx)

                Left ->
                    max 0 (env - tx)


basicEnvelope : Direction -> BasicCollage msg -> Float
basicEnvelope dir basic =
    case basic of
        Core.Shape _ (Core.Polygon ps) ->
            pathEnvelope dir ps

        Core.Shape style (Core.Ellipse rx ry) ->
            boxEnvelope dir (2 * rx) (2 * ry) style.line.thickness

        Core.Path _ (Core.Polyline ps) ->
            pathEnvelope dir ps

        Core.Image _ w h ->
            boxEnvelope dir w h 0

        --FIXME: calculate envelope for Text
        Core.Text _ text ->
            0

        Core.Group forms ->
            --FIXME: correct with translation...
            (List.maximum <| List.map (envelope dir) forms) ? 0

        --FIXME: calculate envelope for Element
        Core.Element _ ->
            0


pathEnvelope : Direction -> List Point -> Float
pathEnvelope dir ps =
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


boxEnvelope : Direction -> Float -> Float -> Float -> Float
boxEnvelope dir width height thickness =
    thickness
        + case dir of
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


{-| Create an empty Collage.
This is useful for getting your spacing right and for making borders.
-}
spacer : Float -> Float -> Collage msg
spacer w h =
    rectangle w h |> styled transparent invisible


{-| An Element that takes up no space. Good for things that appear conditionally:

    flow down [ img1, if showMore then img2 else empty ]

-}
empty : Collage msg
empty =
    spacer 0 0


{-| Given two diagrams a and b, place b to the **left** of a,
such that their origins are on a horizontal line and their envelopes touch.
The origin of the new Collage is the origin of a. FIXME: true?
Sumarised:

    before a b -- read: before a, put b

Which is equivallent to:

    b
        |> before a

-}
before : Collage msg -> Collage msg -> Collage msg
before a b =
    let
        tx =
            (envelope Left a) + (envelope Right b)
    in
        stack [ a, translate ( -tx, 0 ) b ]


{-| Given two diagrams a and b, place b to the **right** of a,
such that their origins are on a horizontal line and their envelopes touch.
The origin of the new Collage is the origin of a. FIXME: true?
Sumarised:

    after a b -- read: after a, put b

  - Warning: The `(|>)` doesn't read well, don't use it!
  - Note: This is called `beside` in the Diagrams library.

-}
after : Collage msg -> Collage msg -> Collage msg
after a b =
    let
        tx =
            (envelope Right a) + (envelope Left b)
    in
        stack [ a, translate ( tx, 0 ) b ]


{-| Given two forms a and b, place b **above** a,
such that their origins are on a vertical line and their envelopes touch.
The origin of the new Collage is the center of a and b. FIXME: true?
Summarised:

    above a b -- read: above a, put b

Which is equivallent to

    b
        |> above a

-}
above : Collage msg -> Collage msg -> Collage msg
above a b =
    let
        ty =
            (envelope Up a) + (envelope Down b)
    in
        -- NOTE: translate b up means **minus** because of switched vertical axis
        stack [ a, translate ( 0, -ty ) b ]


{-| Given two forms a and b, place b **below** a,
such that their origins are on a vertical line and their envelopes touch.
The origin of the new Collage is the center of a and b. FIXME: true?
Summarised:

    below a b -- read: below a, put b

  - Warning: The `(|>)` doesn't read well, don't use it!

-}
below : Collage msg -> Collage msg -> Collage msg
below a b =
    let
        ty =
            (envelope Down a) + (envelope Up b)
    in
        -- NOTE: translate b up means **plus** because of switched vertical axis
        stack [ a, translate ( 0, ty ) b ]


{-| Place a list of Collages next to each other,
such that their origins are along a horizontal line.
The first element in the list will be on the left, the last on the right.

    horizontal [a, b, c]

       +---+---+---+
       | a | b | c |
       +---+---+---+

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

-}
vertical : List (Collage msg) -> Collage msg
vertical =
    List.foldr below empty


{-| Place a list of diagrams on top of each other,
with their origin points stacked on the "out of page" axis.
The first Collage in the list is on top.
This is the same as the `group` operation in the Collage module.

    stack [a, b, c]

        +---+
        | a |
        +---+

-}
stack : List (Collage msg) -> Collage msg
stack =
    --FIXME: why reverse needed? change renderer?
    Collage.group << List.reverse



-- Queries ---------------------------------------------------------------------


width : Collage msg -> Float
width form =
    envelope Left form + envelope Right form


height : Collage msg -> Float
height form =
    envelope Up form + envelope Down form



-- Anchors ---------------------------------------------------------------------


{-| Translate a Collage such that the origin is on the top edge of the bounding box.
-}
north : Collage msg -> Collage msg
north form =
    translate ( 0, envelope Up form ) form


{-| -}
northeast : Collage msg -> Collage msg
northeast =
    north << east


{-| Translate a Collage such that the origin is on the right edge of the bounding box.
-}
east : Collage msg -> Collage msg
east form =
    translate ( -(envelope Right form), 0 ) form


{-| -}
southeast : Collage msg -> Collage msg
southeast =
    south << east


{-| Translate a Collage such that the origin is on the bottom edge of the bounding box.
-}
south : Collage msg -> Collage msg
south form =
    translate ( 0, -(envelope Down form) ) form


{-| -}
southwest : Collage msg -> Collage msg
southwest =
    south << west


{-| Translate a Collage such that the origin is on the left edge of the bounding box.
-}
west : Collage msg -> Collage msg
west form =
    translate ( envelope Left form, 0 ) form


{-| -}
northwest : Collage msg -> Collage msg
northwest =
    north << west


{-| Translate a Collage such that the envelope in all directions is equal.

  - Note: The anchor of every Collage defaults to this.
    Only use this function to "correct" previous translations of the origin.

-}
center : Collage msg -> Collage msg
center form =
    let
        left =
            envelope Left form

        right =
            envelope Right form

        tx =
            (right - left) / 2

        up =
            envelope Up form

        down =
            envelope Down form

        ty =
            (down - up) / 2
    in
        translate ( -tx, ty ) form



-- Debuging --------------------------------------------------------------------


{-| Draw a red dot at `(0, 0)` in the diagram's local vector space.
-}
showOrigin : Collage msg -> Collage msg
showOrigin form =
    let
        origin =
            circle 3
                |> filled (uniform Color.red)
                |> translate form.origin
    in
        stack [ origin, form ]


{-| Draw a red dot box around a diagram.
-}
showEnvelope : Collage msg -> Collage msg
showEnvelope form =
    let
        outline =
            rectangle (width form) (height form)
                |> outlined (dot 2 (uniform Color.red))
                |> translate form.origin
    in
        stack [ outline, form ]
