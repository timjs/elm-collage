module Collage.Layout
    exposing
        ( Direction
        , above
        , after
        , align
        , at
        , base
        , before
        , below
        , beside
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

{-| TODO

@docs Direction, envelope, width, height
@docs spacer, empty
@docs place, beside, before, after, above, below, horizontal, vertical, stack, impose
@docs align, center, at, top, topRight, right, bottomRight, bottom, bottomLeft, left, topLeft, base
@docs showOrigin, showEnvelope

-}

import Collage exposing (..)
import Collage.Core as Core
import Color
import Maybe.Extra exposing ((?))
import Text
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

        Core.Text text ->
            let
                raw =
                    Text.toRawSvg text
            in
            handleBox dir (rotate (Core.calculateDimensions raw))

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

  - Note: `beside dir` and all its descendents forms a monoid with `empty`.
  - Note: same as `beside` in Diagrams

-}
beside : Direction -> Collage msg -> Collage msg -> Collage msg
beside dir a b =
    stack [ a, place dir a b ]


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
The origin of the new Collage is the center of a and b. FIXME: true?
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
The origin of the new Collage is the center of a and b. FIXME: true?
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


{-| Place a list of diagrams on top of each other,
with their origin points stacked on the "out of page" axis.
The first Collage in the list is on top.
This is the same as the `group` operation in the Collage module.

    stack [a, b, c]

        +---+
        | a |
        +---+

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

`impose collage background` stacks `collage` on `background`.
The background will be used to calculate envlopes.

       impose a b -- read: "impose a on b"

or:

       b
           |> impose a

-}
impose : Collage msg -> Collage msg -> Collage msg
impose front back =
    Core.collage <| Core.Subcollage front back



-- Queries ---------------------------------------------------------------------


{-| -}
width : Collage msg -> Float
width collage =
    envelope Left collage + envelope Right collage


{-| -}
height : Collage msg -> Float
height collage =
    envelope Up collage + envelope Down collage



-- Alignment ---------------------------------------------------------------------


type alias Anchor msg =
    Collage msg -> Point


{-| Shift a Collage such that the origin is on the given anchor.

    List.map (align north)

Anchors are created by functions from section below.

-}
align : Anchor msg -> Collage msg -> Collage msg
align anchor collage =
    shift (anchor collage) collage


{-| Shift a Collage such that the envelope in all directions is equal.

Alias for `align base`.

-}
center : Collage msg -> Collage msg
center =
    align base


{-| Stack a collage on top of a specified anchor of a host.

Makes placing objects on a collage easier:

    drawing
        |> at south dot
        |> at norhteast dot

        +---------0
        | drawing |
        +----0----+

instead of:

    stack
        [ dot
        , align west <| stack
            [ dot
            , align east drawing
            ]
        ]

-}
at : Anchor msg -> Collage msg -> Collage msg -> Collage msg
at anchor collage host =
    stack [ collage, align anchor host ]



-- Anchors -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-|

        +-X-+
        |   |
        +---+

-}
top : Anchor msg
top collage =
    ( 0, envelope Up collage )


{-|

        +---X
        |   |
        +---+

-}
topRight : Anchor msg
topRight collage =
    ( -(envelope Right collage), envelope Up collage )


{-|

        +---+
        |   X
        +---+

-}
right : Anchor msg
right collage =
    ( -(envelope Right collage), 0 )


{-|

        +---+
        |   |
        +---X

-}
bottomRight : Anchor msg
bottomRight collage =
    ( -(envelope Right collage), -(envelope Down collage) )


{-|

        +---+
        |   |
        +-X-+

-}
bottom : Anchor msg
bottom collage =
    ( 0, -(envelope Down collage) )


{-|

        +---+
        |   |
        X---+

-}
bottomLeft : Anchor msg
bottomLeft collage =
    ( envelope Left collage, -(envelope Down collage) )


{-|

        +---+
        X   |
        +---+

-}
left : Anchor msg
left collage =
    ( envelope Left collage, 0 )


{-|

        X---+
        |   |
        +---+

-}
topLeft : Anchor msg
topLeft collage =
    ( envelope Left collage, envelope Up collage )



-- TODO: add baseX, and baseY (horizontalBase and vertialBase)?
-- TODO: and add centerX, and centerY too?


{-|

        +---+
        | X |
        +---+

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


{-| Draw a red dot box around the collage representing the envelope.
-}
showEnvelope : Collage msg -> Collage msg
showEnvelope collage =
    --FIXME: add
    -- OR
    --  |> shift collage.origin
    -- after stacking to fix frame drawing
    let
        outline =
            rectangle (width collage) (height collage)
                |> outlined (dot 2 (uniform Color.red))
                |> shift collage.origin
    in
    stack [ outline, collage ]
