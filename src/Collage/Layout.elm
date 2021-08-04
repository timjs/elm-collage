module Collage.Layout exposing
    ( envelope, Direction(..), facing, distances, Distances, width, height
    , horizontal, vertical, stack, impose, beside, place
    , spacer, empty
    , align, at, center
    , Anchor, top, topRight, right, bottomRight, bottom, bottomLeft, left, topLeft, base
    , name, locate, connect, names
    , showOrigin, showEnvelope, debug
    )

{-| With this module, you can compose collages in a more automatic way.
Instead of shifting collages manually,
this module figures out the dimensions of each part of your drawing
and places them seamlessly next to each other, above each other or on top of each other.
This is all possible by keeping track of the _envelope_ or _bounding box_ of each collage.

Collages are positioned relative to their _internal origin_.
By default, a collage's internal origin lays in its exact center.
By _shifting_ a collage, we change its internal origin
and the way it composes with other collages.
Another method to change the origin is by using _alignments_.

All these ideas are based on the [Diagrams library](https://archives.haskell.org/projects.haskell.org/diagrams/) for Haskell
and the [Scalable Graphics library](https://dl.acm.org/citation.cfm?id=2746329) for Clean.
You can regard this module as a simplified version of above libraries.


### Contents

  - [Envelopes](#envelopes)
  - [Layouting](#layouting)
  - [Spacers](#spacers)
  - [Aligning](#aligning)
      - [Anchors](#anchors)
  - [Naming](#naming)
  - [Debugging](#debugging)


# Envelopes

An _envelope_ defines the bounding box of a collage.
It is similar to a _bounding box_.
[Envelopes](https://archives.haskell.org/projects.haskell.org/diagrams/doc/manual.html#envelopes-and-local-vector-spaces)
can become quite complex when we calculate them in all possible directions.
Here, we restrict ourselves to four: up, down, right and left.

Envelopes answer the question:
“If I want to put my collage in a rectangular envelope,
what are the distances from its local origin to the envelope edges?”

@docs envelope, Direction, facing, distances, Distances, width, height


# Layouting

Collages can be composed in three ways:

  - horizontally, or next to each other
  - vertically, or above each other
  - on the "out of page" axis, or stacked on top of each other

A more advanced way of combining collages is by _imposition_:
placing one collage on top of a background and forgetting about the dimensions of the foreground.
All ways of combining are expressed used one basic placing function,
which you can use in some extreme cases where you need to place a collage but not combine it with others.

@docs horizontal, vertical, stack, impose, beside, place


# Spacers

What is placing collages without being able to space them accordingly?
These two collages are invisible and only take up some space.
(Or they even don't...)

@docs spacer, empty


# Aligning

@docs align, at, center


## Anchors

@docs Anchor, top, topRight, right, bottomRight, bottom, bottomLeft, left, topLeft, base


# Naming

**Warning!**

  - This part of the library is **highly experimental**.
    Your mileage may vary.
  - Giving multiple names to the _same_ collage will overwrite the old one.
  - The library _will not_ prevent you from using the same name twice for _different_ collages.
    You are yourself responsible for not using duplicate names!

@docs name, locate, connect, names


# Debugging

These two functions can aid in discovering how collages are composed.

@docs showOrigin, showEnvelope, debug


# Possible extensions

We can imagine some possible extensions which could aid in designing composable drawings:

  - way to add padding
  - add `baseX`, and `baseY` (`horizontalBase` and `vertialBase`)?
  - and add `centerX`, and `centerY` too?
  - ...

-}

import Collage exposing (..)
import Collage.Core as Core
import Collage.Super exposing (..)
import Color
import Dict exposing (Dict)
import Helpers
import Maybe exposing (withDefault)



-- Directions ------------------------------------------------------------------


{-| The four different directions in which we can calculate an envelope.
-}
type Direction
    = Up
    | Down
    | Right
    | Left


{-| Calculate the facing direction.

    Up <-> Down

    Left <-> Right

-}
facing : Direction -> Direction
facing dir =
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
We represent the origin with `(X)`.
When calling, for example, `envelope Up`,
we calculate the distance from `(X)` to the upper edge of the rectangle.

        +–––––––––––––––+
        |       ˄       |
        |    Up |       |
        |       | Right |
        | ˂––––(X)––––˃ |
        |  Left |       |
        |       | Down  |
        |       ˅       |
        +–––––––––––––––+

The same holds for the other three directions.

-}
envelope : Direction -> Collage msg -> Float
envelope dir col =
    let
        { toTop, toBottom, toLeft, toRight } =
            distances col
    in
    case dir of
        Up ->
            toTop

        Down ->
            toBottom

        Right ->
            toRight

        Left ->
            toLeft



-- Distances -------------------------------------------------------------------


{-| Type alias collecting envelope distances in all four directions.
-}
type alias Distances =
    { toTop : Float
    , toBottom : Float
    , toRight : Float
    , toLeft : Float
    }


{-| Unpack a distances record in a list of points representing the corners of the envelope.
-}
unpack : Distances -> List Point
unpack { toTop, toBottom, toRight, toLeft } =
    [ ( -toLeft, -toBottom )
    , ( toRight, -toBottom )
    , ( toRight, toTop )
    , ( -toLeft, toTop )
    ]


{-| Calculate the envelope in all four directions at once.

The result is a `Distances` record with `toTop`, `toBottom`, `toLeft`, and `toRight` fields.
Use this function if you need envelopes in multiple directions at the same time.

    { toTop, toBottom } = distances collage
    ...use up and down...

-}
distances : Collage msg -> Distances
distances col =
    let
        points =
            handleBasic col.basic

        ( xs, ys ) =
            points
                |> List.map (Core.apply col)
                |> List.unzip
    in
    --FIXME: maybe not very efficent to do this here?
    { toTop = List.maximum ys |> withDefault 0
    , toBottom = -(List.minimum ys |> withDefault 0)
    , toRight = List.maximum xs |> withDefault 0
    , toLeft = -(List.minimum xs |> withDefault 0)
    }


handleBasic : BasicCollage msg -> List Point
handleBasic basic =
    case basic of
        -- Shapes --
        Core.Shape ( _, { thickness } ) (Core.Circle r) ->
            let
                d =
                    2 * r
            in
            handleBox thickness ( d, d )

        Core.Shape ( _, { thickness } ) (Core.Ellipse rx ry) ->
            handleBox thickness ( 2 * rx, 2 * ry )

        Core.Shape ( _, { thickness } ) (Core.Rectangle w h _) ->
            handleBox thickness ( w, h )

        Core.Shape ( _, { thickness } ) (Core.Polygon ps) ->
            handlePoints thickness ps

        Core.Shape ( _, line ) (Core.Loop path) ->
            --NOTE: Use the same calculations as for paths
            handleBasic (Core.Path line path)

        -- Paths --
        Core.Path { thickness, cap } (Core.Polyline ps) ->
            handlePoints
                (if cap == Flat then
                    0

                 else
                    thickness
                )
                ps

        -- Boxes --
        Core.Text dims _ ->
            handleBox 0 dims

        Core.Image dims _ ->
            handleBox 0 dims

        Core.Html dims _ _ ->
            handleBox 0 dims

        -- Groups --
        Core.Group cols ->
            cols
                |> List.map (distances >> unpack)
                |> List.concat
                |> handlePoints 0

        Core.Subcollage _ back ->
            --NOTE: We ignore the foreground and only calculate the distances of the background
            --NOTE: We have to handle the rotation, this is done by `distances`
            distances back
                |> unpack
                |> handlePoints 0


handlePoints : Float -> List Point -> List Point
handlePoints thickness =
    let
        thicken ( x, y ) =
            let
                t =
                    thickness / 2
            in
            ( if x < 0 then
                x - t

              else
                x + t
            , if y < 0 then
                y - t

              else
                y + t
            )
    in
    List.map thicken


handleBox : Float -> ( Float, Float ) -> List Point
handleBox thickness ( w, h ) =
    let
        x =
            w / 2

        y =
            h / 2
    in
    handlePoints thickness
        [ ( -x, -y )
        , ( x, -y )
        , ( x, y )
        , ( -x, y )
        ]



-- Queries ---------------------------------------------------------------------


{-| Calculates the width of a collage.

The width is equivalent to the envelopes in the left and right directions:

    width collage == envelope Left collage + envelope Right collage

-}
width : Collage msg -> Float
width col =
    let
        { toLeft, toRight } =
            distances col
    in
    toLeft + toRight


{-| Calculates the height of a collage.

The height is equivalent to the envelopes in the up and down directions:

    height collage == envelope Up collage + envelope Down collage

-}
height : Collage msg -> Float
height col =
    let
        { toTop, toBottom } =
            distances col
    in
    toTop + toBottom



-- Layouts ---------------------------------------------------------------------
-- Phantom collages -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| Create an empty collage of given width and height.

This is useful for getting your spacing right and for making borders.

    hspace = spacer 10 0

    horizontal <| List.intersperse hspace [a, b, c]

        +–––+ +–––+ +–––+
        | a | | b | | c |
        +–––+ +–––+ +–––+

-}
spacer : Float -> Float -> Collage msg
spacer w h =
    rectangle w h |> styled ( transparent, invisible )


{-| A collage that takes up no space. Good for things that appear conditionally:

    horizontal [ a, if showMore then b else empty ]

        +–––+– – – – – –+
        |(a)|  maybe b? |
        +–––+– – – – – –+

  - Note: this is the identity element of the monoid on collages.

-}
empty : Collage msg
empty =
    spacer 0 0



-- Placing -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| Shift a collage in a direction to precisely that position where it would touch the first one,
without composing them.

Use this to position a collage next to another collage without actually composing them.

  - Note: called `juxtapose` in Diagrams.

-}
place : Direction -> Collage msg -> Collage msg -> Collage msg
place dir a b =
    let
        len =
            envelope dir a + envelope (facing dir) b

        move =
            case dir of
                Up ->
                    ( 0, len )

                Down ->
                    ( 0, -len )

                Right ->
                    ( len, 0 )

                Left ->
                    ( -len, 0 )
    in
    shift move b


{-| Place a collage _beside_ another one in the given direction and combine them into a new one.

Most of the time it is way nicer to use `horizontal` or `vertical` to express your layout,
but `beside dir` can come into hand as a curried function.

The new origin will be the origin of the first argument.

  - Note: `beside dir` forms a monoid with `empty` for every `dir`.
  - Note: same as `beside` in Diagrams.

-}
beside : Direction -> Collage msg -> Collage msg -> Collage msg
beside dir a b =
    stack [ a, place dir a b ]



-- Combining -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -


{-| Place a list of collages next to each other,
such that their origins are along a horizontal line.
The first element in the list will be on the left, the last on the right.

    horizontal [a, b, c]

        +–––+–––+–––+
        |(a)| b | c |
        +–––+–––+–––+

The new origin will be the origin of the first element in the list.

  - Note: this is called `hcat` in Diagrams.

-}
horizontal : List (Collage msg) -> Collage msg
horizontal =
    List.foldr (beside Right) empty


{-| Place a list of collages next to each other,
such that their origins are along a vertical line.
The first element in the list will be on the top, the last on the bottom.

    vertical [a, b, c]

        +–––+
        |(a)|
        +–––+
        | b |
        +–––+
        | c |
        +–––+

The new origin will be the origin of the first element in the list.

  - Note: this is called `vcat` in Diagrams.

-}
vertical : List (Collage msg) -> Collage msg
vertical =
    List.foldr (beside Down) empty


{-| Place a list of collages on top of each other, with their origin points stacked on the "out of page" axis.

The first collage in the list is on top.
This actually is the same as the `group` operation in the Collage module.

    stack [a, b, c]

        +–––+
        |(a)|
        +–––+

(Yes, `b` and `c` are somewhere below `a`...)

The new origin will be the origin of the first element in the list.

  - Note: this is called `concat` in Diagrams.

  - Note: when we create an operator `(<>)` like

        (<>) a b =
            stack [ a, b ]

    then `(<>)` forms a monoid together with `empty`.
    `(<>)` is called `atop` in Diagrams.

-}
stack : List (Collage msg) -> Collage msg
stack =
    Collage.group


{-| Impose a collage on a background.

The call

    impose fore back

stacks `fore` on `back`.
The envelope of `fore` will be "forgotten"
and `back` will be used to calculate the envelope of the resulting collage.

        +–––––––––––+
        |(b)+–––+   |
        |   | a |   | <-- new envelope
        |   +–––+   |
        +–––––––––––+

Obviously, this also works with the background having a smaller envelope than the foreground.

The new origin will be the origin of the background.

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

        +–(X)–+––––X––––+–X–+
        |  a  |    b    | c |
        |     +–––––––––+   |
        +–––––+         |   |
                        +–––+

Anchors are created by the functions from the section below.

-}
align : Anchor msg -> Collage msg -> Collage msg
align anchor col =
    shift (Collage.opposite <| anchor col) col


{-| Stack a collage on top of a specified anchor of a host.

Makes placing objects on a collage a lot easier:

    collage
        |> at bottom dot
        |> at upperRight dot

        +–––––––––0
        | collage |
        +––––0––––+

instead of:

    stack
        [ dot
        , align upperRight <|
            stack
                [ dot
                , align bottom collage
                ]
        ]

This does not change the origin of `collage`.

-}
at : Anchor msg -> Collage msg -> Collage msg -> Collage msg
at anchor fore back =
    stack
        [ fore
            |> shift (anchor back)
        , back
        ]


{-| Shift a collage such that the envelope in all directions is equal.

This is the same as aligning on the base anchor:

    center collage == align base collage

-}
center : Collage msg -> Collage msg
center =
    align base



-- Anchors -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| Anchors are functions which calculate a point relative to the origin of a given collage.
-}
type alias Anchor msg =
    Collage msg -> Point


{-|

        +(X)+
        |   |
        +–––+

-}
top : Anchor msg
top col =
    let
        { toTop } =
            distances col
    in
    ( 0, toTop )


{-|

        +––(X)
        |   |
        +–––+

-}
topRight : Anchor msg
topRight col =
    let
        { toRight, toTop } =
            distances col
    in
    ( toRight, toTop )


{-|

        +–––+
        |  (X)
        +–––+

-}
right : Anchor msg
right col =
    let
        { toRight } =
            distances col
    in
    ( toRight, 0 )


{-|

        +–––+
        |   |
        +––(X)

-}
bottomRight : Anchor msg
bottomRight col =
    let
        { toRight, toBottom } =
            distances col
    in
    ( toRight, -toBottom )


{-|

        +–––+
        |   |
        +(X)+

-}
bottom : Anchor msg
bottom col =
    let
        { toBottom } =
            distances col
    in
    ( 0, -toBottom )


{-|

        +–––+
        |   |
       (X)––+

-}
bottomLeft : Anchor msg
bottomLeft col =
    let
        { toLeft, toBottom } =
            distances col
    in
    ( -toLeft, -toBottom )


{-|

        +–––+
       (X)  |
        +–––+

-}
left : Anchor msg
left col =
    let
        { toLeft } =
            distances col
    in
    ( -toLeft, 0 )


{-|

       (X)––+
        |   |
        +–––+

-}
topLeft : Anchor msg
topLeft col =
    let
        { toLeft, toTop } =
            distances col
    in
    ( -toLeft, toTop )


{-|

        +–––+
        |(X)|
        +–––+

-}
base : Anchor msg
base col =
    let
        { toTop, toBottom, toLeft, toRight } =
            distances col

        tx =
            (toRight - toLeft) / 2

        ty =
            (toTop - toBottom) / 2
    in
    ( tx, ty )



-- Naming ----------------------------------------------------------------------


{-| Give a name to (a part of) a collage in order to locate it after composition.
-}
name : String -> Collage msg -> Collage msg
name string col =
    { col | name = Just string }


{-| Locate a named part of a collage and calculate the coordinates using the given anchor in the new coordinate system.

First be sure to give a name to a sub collage using `name`,
only after that you can retrieve the collage using this function.
Well, you cannot retrieve the whole collage,
but a point calculated _relative to its internal origin_ using an anchor.
This point is then subjected to the samen transformations as all the groups above it.

When a collage part could not be found,
we display a message on the console for your convenience.

-}
locate : String -> Anchor msg -> Collage msg -> Maybe Point
locate string anchor this =
    let
        recurse col =
            let
                match =
                    Maybe.map ((==) string) col.name |> withDefault False

                firstOf =
                    --NOTE: This saves us recursing down when we found what we're looking for!
                    --FIXME: This is depth first!!!
                    Helpers.foldrLazy (Helpers.orLazy << recurse) Nothing
            in
            if match then
                Just <| anchor col

            else
                Maybe.map (Core.apply col) <|
                    case col.basic of
                        Core.Group cols ->
                            firstOf cols

                        Core.Subcollage fore back ->
                            firstOf [ fore, back ]

                        _ ->
                            Nothing
    in
    recurse this


{-| Breadth-first search on collages

Locating is done in breadth first order:
i.e. left-to-right in horizontal compositions,
top-to-bottom in vertical compositions,
or front-to-back in stacked compositions
and after that going deeper down, descending into subcollages.

-}
locate_ : String -> Anchor msg -> Collage msg -> Maybe Point
locate_ string anchor this =
    let
        recurse queue =
            case queue of
                [] ->
                    Nothing

                col :: rest ->
                    let
                        match =
                            Maybe.map ((==) string) col.name |> withDefault False

                        update =
                            List.map (Core.combine col)
                    in
                    if match then
                        --NOTE: We found it!
                        Just <| anchor col

                    else
                        --NOTE: We go on with our search and keep track of the transformations
                        case col.basic of
                            Core.Group cols ->
                                --NOTE: First recurse on the rest of the queue, then go for the group contents
                                recurse (rest ++ update cols)

                            Core.Subcollage fore back ->
                                recurse (rest ++ update [ fore, back ])

                            _ ->
                                recurse rest
    in
    recurse [ this ]


{-| Return a dictionary with all named parts of given collage.
-}
names : Collage msg -> Dict String (Collage msg)
names =
    let
        recurse col res =
            case col.name of
                Just n ->
                    Dict.insert n col res

                Nothing ->
                    res
    in
    --NOTE: We use `foldr` here so named collages "higher up" will overwrite those down in the hierarchy.
    Core.foldr recurse Dict.empty


{-| Connect a list of points which are located inside a collage.

For named parts that could not be found,
the result will be _ignored_.

-}
connect : List ( String, Anchor msg ) -> LineStyle -> Collage msg -> Collage msg
connect locations line col =
    let
        positions =
            locations
                |> List.map (\( n, a ) -> locate n a col)
                |> Helpers.values
    in
    impose (path positions |> traced line) col



-- Debuging --------------------------------------------------------------------


{-| Draw a red dot at the local origin of the collage.
-}
showOrigin : Collage msg -> Collage msg
showOrigin col =
    let
        origin =
            circle 3
                |> filled (uniform Color.red)
                |> name "_origin_"
    in
    impose origin col


{-| Draw a red dotted box around the collage representing the envelope.
-}
showEnvelope : Collage msg -> Collage msg
showEnvelope col =
    let
        outline =
            rectangle (width col) (height col)
                |> outlined (dot 2 (uniform Color.red))
                |> shift (base col)
                |> name "_envelope_"
    in
    impose outline col


{-| Show both the envelope and the origin of a collage.
-}
debug : Collage msg -> Collage msg
debug =
    showEnvelope >> showOrigin
