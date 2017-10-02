module Collage.Layout
    exposing
        ( Anchor
        , Direction(..)
        , align
        , at
        , base
        , bottom
        , bottomLeft
        , bottomRight
        , center
        , empty
        , envelope
        , height
        , horizontal
        , impose
        , left
        , opposite
        , place
        , right
        , showEnvelope
        , showOrigin
        , spacer
        , stack
        , top
        , topLeft
        , topRight
        , vertical
        , width
        )

{-|


# Layouting of collages

With this module you can compose collages in a more automatic way.
You can lay out collages in multiple ways:

  - horizontally, next to each other
  - vertically, above each other
  - stack them on top of each other

This is all possible by keeping track of the _envelope_ or _bounding box_ of each collage.
Because we know of each basic shape how wide and how tall they are,
we can place two collages next to each other, making them touch.

Each collage has a internal origin.
The envelope is calculated relative to this origin.
By shifting a collage, we change the origin and the way collages are composed.

This ideais based on the [Diagrams library](https://archives.haskell.org/projects.haskell.org/diagrams/) for Haskell
and the [Scalable Graphics library](https://dl.acm.org/citation.cfm?id=2746329) for Clean.
You can regard this module as a simplified version of above libraries.

An _envelope_ defines the bounding box of a collage.
It is sometimes referred to as a _span box_.
[Envelopes](https://archives.haskell.org/projects.haskell.org/diagrams/doc/manual.html#envelopes-and-local-vector-spaces)
can become quite complex when we calculate them in all possible directions.
Here, we restrict ourselves to calculate the envelope in four different _directions_: up, down, right and left.


## Envelopes

@docs envelope, Direction, opposite, width, height


## Layouting

@docs horizontal, vertical, stack, impose, place


## Aligning

@docs align, at, center


### Anchors

@docs top, topRight, right, bottomRight, bottom, bottomLeft, left, topLeft, base, Anchor


## Spacers

@docs spacer, empty


## Debugging

These two functions can aid in discovering how collages are composed.

@docs showOrigin, showEnvelope


## Extensions

We can imagine some possible extensions which could aid in designing composable drawings:

  - add padding
  - ...

-}

import Collage exposing (..)
import Collage.Core as Core
import Color
import Maybe.Extra exposing ((?))
import Tuple exposing (first, second)


-- Directions ------------------------------------------------------------------


{-| The four different directions in which we can calculate an envelope.
-}
type Direction
    = Up
    | Down
    | Right
    | Left


{-| Calculate the opposite direction.
-}
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


{-| Calculate the envelope of a collage relative to its internal origin.

The figure below illustrates the four distances that can be calculated.
We represent the origin with `X`.
When calling, for example, `envelope Up`,
we calculate the distance from `X` to the upper edge of the rectangle.

        +–––––––––––––––+
        |       ˄       |
        |    Up |       |
        |       | Right |
        | ˂–––– X ––––˃ |
        |  Left |       |
        |       | Down  |
        |       ˅       |
        +–––––––––––––––+

The same holds for the other three directions.

-}
envelope : Direction -> Collage msg -> Float
envelope dir collage =
    let
        env =
            handleBasic dir collage.theta collage.basic

        ( tx, ty ) =
            collage.origin
    in
    collage.scale
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

        Core.Shape ( _, line ) (Core.ClosedPath path) ->
            -- Use the same calculations as for paths
            handleBasic dir theta (Core.Path line path)

        Core.Path line (Core.Polyline ps) ->
            handlePath dir (List.map rotate ps)

        Core.Text dims _ ->
            handleBox dir (rotate dims)

        Core.Image dims _ ->
            handleBox dir (rotate dims)

        Core.Element dims _ ->
            handleBox dir (rotate dims)

        Core.Group forms ->
            --FIXME: correct with translation???
            (List.maximum <| List.map (envelope dir) forms) ? 0

        Core.Subcollage _ back ->
            --NOTE: we ignore the foreground and only calculate the envelope of the background
            --FIXME: add rotation
            envelope dir back


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



-- Queries ---------------------------------------------------------------------


{-| Calculates the width of a collage.

The width is equivalent to the envlopes in the left and right directions:

    width collage == envelope Left collage + envelope Right collage

-}
width : Collage msg -> Float
width collage =
    envelope Left collage + envelope Right collage


{-| Calculates the height of a collage.

The height is equivalent to the envlopes in the up and down directions:

    height collage == envelope Up collage + envelope Down collage

-}
height : Collage msg -> Float
height collage =
    envelope Up collage + envelope Down collage



-- Layouts ---------------------------------------------------------------------
-- Phantom collages -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| Create an empty collage of given width and height.

This is useful for getting your spacing right and for making borders.

-}
spacer : Float -> Float -> Collage msg
spacer w h =
    rectangle w h |> styled ( transparent, invisible )


{-| An Element that takes up no space. Good for things that appear conditionally:

    horizontal [ img1, if showMore then img2 else empty ]

  - Note: this is the identity element of the monoid on collages.

-}
empty : Collage msg
empty =
    spacer 0 0



-- Placing -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| Shift a collage in a direction to precicly that position where it would touch the first one.

Use this to position a collage next to another collage without actually composing them.

  - Note: called `juxtapose` in Diagrams.

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

  - Note: `beside dir` and all its descendents forms a monoid with `empty`.
  - Note: same as `beside` in Diagrams

-}
beside : Direction -> Collage msg -> Collage msg -> Collage msg
beside dir a b =
    stack [ a, place dir a b ]


{-| Given two diagrams a and b, place b to the **left** of a,
such that their origins are on a horizontal line and their envelopes touch.
The origin of the new collage is the origin of a. FIXME: true?
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
The origin of the new collage is the origin of a. FIXME: true?
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
The origin of the new collage is the center of a and b. FIXME: true?
Summarised:

    above a b -- read: above a, put b

Which is equivallent to

    b
        |> above a

  - Warning: Do not read this infix, arguments are swapped with regard to Diagrams!

-}
above : Collage msg -> Collage msg -> Collage msg
above =
    beside Up


{-| Given two forms a and b, place b **below** a,
such that their origins are on a vertical line and their envelopes touch.
The origin of the new collage is the center of a and b. FIXME: true?
Summarised:

    below a b -- read: below a, put b

  - Warning: The `(|>)` doesn't read well combined with `below`, don't use it!
  - Warning: Do not read this infix, arguments are swapped with regard to Diagrams!

-}
below : Collage msg -> Collage msg -> Collage msg
below =
    beside Down



-- Combining -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -


{-| Place a list of collages next to each other,
such that their origins are along a horizontal line.
The first element in the list will be on the left, the last on the right.

    horizontal [a, b, c]

       +–––+–––+–––+
       | a | b | c |
       +–––+–––+–––+

  - Note: this is called `hcat` in Diagrams

-}
horizontal : List (Collage msg) -> Collage msg
horizontal =
    List.foldr after empty


{-| Place a list of Diagrams next to each other,
such that their origins are along a vertical line.
The first element in the list will be on the top, the last on the bottom.

    vertical [a, b, c]

       +–––+
       | a |
       +–––+
       | b |
       +–––+
       | c |
       +–––+

  - Note: this is called `vcat` in Diagrams

-}
vertical : List (Collage msg) -> Collage msg
vertical =
    List.foldr below empty


{-| Place a list of diagrams on top of each other,
with their origin points stacked on the "out of page" axis.
The first collage in the list is on top.
This is the same as the `group` operation in the collage module.

    stack [a, b, c]

        +–––+
        | a |
        +–––+

(Yes, `b` and `c` are somewhere below `a`...)

  - Note: this is called `concat` in Diagrams.

-}
stack : List (Collage msg) -> Collage msg
stack =
    Collage.group


{-| Infix operator for `stack`.

    a <> b == stack [a, b]

  - Note: `(<>)` forms a monoid with `empty`.
  - Note: this is called `(<>)` or `atop` in Diagrams.

-}
(<>) : Collage msg -> Collage msg -> Collage msg
(<>) a b =
    stack [ a, b ]


{-| Impose one collage on a background.

`impose foreground background` stacks `foreground` on `background`.
The envelope of `foreground` will be "forgotten"
and `background` will be used to calculate the envlope of the resulting collage.

       impose a b -- read: "impose a on b"

or:

       b
           |> impose a

-}
impose : Collage msg -> Collage msg -> Collage msg
impose front back =
    Core.collage <| Core.Subcollage front back



-- Alignment -------------------------------------------------------------------


{-| Shift a collage such that the origin is on the given anchor.

Use this, for example, when you like to align some collages to the top:

    [a, b, c]
        |> List.map (align top)
        |> horizontal

    =>

    +––X––+––––X––––+–X–+
    |  a  |    b    | c |
    |     +–––––––––+   |
    +–––––+         |   |
                    +–––+

Anchors are created by the functions from the section below.

-}
align : Anchor msg -> Collage msg -> Collage msg
align anchor collage =
    shift (anchor collage) collage


{-| Stack a collage on top of a specified anchor of a host.

Makes placing objects on a collage a lot easier:

    drawing
        |> at bottom dot
        |> at upperRight dot

        +–––––––––0
        | drawing |
        +––––0––––+

instead of:

    stack
        [ dot
        , align upperRight <| stack
            [ dot
            , align bottom drawing
            ]
        ]

-}
at : Anchor msg -> Collage msg -> Collage msg -> Collage msg
at anchor collage host =
    stack [ collage, align anchor host ]


{-| Shift a collage such that the envelope in all directions is equal.

This is the same as aligning on the base anchor:

    center collage == align base collage

-}
center : Collage msg -> Collage msg
center =
    align base



-- Anchors -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| Anchors are technically just functions.

You can use any function as an anchor as long as it takes a collage and returns a displacement relative to its internal origin.

-}
type alias Anchor msg =
    Collage msg -> Point


{-|

        +-X-+
        |   |
        +–––+

-}
top : Anchor msg
top collage =
    ( 0, envelope Up collage )


{-|

        +–––X
        |   |
        +–––+

-}
topRight : Anchor msg
topRight collage =
    ( -(envelope Right collage), envelope Up collage )


{-|

        +–––+
        |   X
        +–––+

-}
right : Anchor msg
right collage =
    ( -(envelope Right collage), 0 )


{-|

        +–––+
        |   |
        +–––X

-}
bottomRight : Anchor msg
bottomRight collage =
    ( -(envelope Right collage), -(envelope Down collage) )


{-|

        +–––+
        |   |
        +-X-+

-}
bottom : Anchor msg
bottom collage =
    ( 0, -(envelope Down collage) )


{-|

        +–––+
        |   |
        X–––+

-}
bottomLeft : Anchor msg
bottomLeft collage =
    ( envelope Left collage, -(envelope Down collage) )


{-|

        +–––+
        X   |
        +–––+

-}
left : Anchor msg
left collage =
    ( envelope Left collage, 0 )


{-|

        X–––+
        |   |
        +–––+

-}
topLeft : Anchor msg
topLeft collage =
    ( envelope Left collage, envelope Up collage )



-- TODO: add baseX, and baseY (horizontalBase and vertialBase)?
-- TODO: and add centerX, and centerY too?


{-|

        +–––+
        | X |
        +–––+

-}
base : Anchor msg
base collage =
    let
        left =
            envelope Left collage

        right =
            envelope Right collage

        tx =
            (right - left) / 2

        up =
            envelope Up collage

        down =
            envelope Down collage

        ty =
            (down - up) / 2
    in
    ( -tx, -ty )



-- Debuging --------------------------------------------------------------------


{-| Draw a red dot at the local origin of the collage.
-}
showOrigin : Collage msg -> Collage msg
showOrigin collage =
    let
        origin =
            circle 3
                |> filled (uniform Color.red)
    in
    stack [ origin, collage ]


{-| Draw a red dotted box around the collage representing the envelope.
-}
showEnvelope : Collage msg -> Collage msg
showEnvelope collage =
    let
        outline =
            rectangle (width collage) (height collage)
                |> outlined (dot 2 (uniform Color.red))
                |> shift collage.origin
    in
    stack [ outline, collage ]
