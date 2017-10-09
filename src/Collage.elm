module Collage
    exposing
        ( BasicCollage
          --(.)
        , Collage
          --(.)
        , FillStyle
          --(.)
        , LineCap(..)
        , LineJoin(..)
        , LineStyle
        , Path
          --(.)
        , Point
        , Shape
          --(.)
        , Style
        , broken
        , circle
        , dash
        , dashdot
        , dot
        , ellipse
        , embedded
        , filled
        , group
        , included
        , invisible
        , line
        , longdash
        , loop
        , ngon
        , opacity
        , opposite
        , outlined
        , path
        , polygon
        , rectangle
        , rendered
        , rotate
        , scale
        , segment
        , semithick
        , shift
        , solid
        , square
        , styled
        , thick
        , thin
        , traced
        , transparent
        , triangle
        , ultrathick
        , ultrathin
        , uniform
        , verythick
        , verythin
        )

{-| The collage module is here to help you create freeform graphics.
You can style all sorts of forms including shapes, paths, text and images,
and then shift, rotate, scale, and group them.


### Coordinate system

Collages use the same coordinate system you might see in an algebra or physics problem.
The origin (0,0) is **at the center** of the collage, not the top left corner as in some other graphics libraries.
Furthermore, the **y-axis points up**: so moving a collage 10 units in the y-axis will move it up on screen.
This is intentional.
The goal is to provide an elegant interface which is abstracted as much as possible from implementation details.


### Creating graphics

To create a graphic you start with creating some form: a _shape_, a _path_ or a chunk of _text_.
Now you can _style_ a form.
It depends on the kind of form you created how you can style it.
A shape, for example can be filled with an uniform color,
outlined with a dotted line,
or both;
a path only can be traced with a line style;
and a piece of text can be made monospaced, bold, italic, underlined etc.
You can think of a form as some kind of _stencil_,
dipping it in different colors of ink and stamp it onto the screen once or multiple times.

Styling a form will turn it into a _collage_.
Collages are the most powerfull object of this library.
They cannot be styled any more,
but they can be shifted, rotated, scaled, made partially transparent, and grouped into a new collage.
Yeah, you read that correctly:
you can group multiple collages into a new one,
which you can shift, rotate, scale, and group again!

So rembember:
before you can transform you drawing,
you have to style it (i.e. turn it into a _collage_),
only after styling you can shift, rotate, scale, etc.
Including an external image or a piece of raw Html also belongs to the possiblilities.


### Interactive graphics

You can make your collages interactive by using the events from the Collage.Event module.
See the documentation of that module for more information.


### Automatic relative positioning

The Collage.Layout module is designed to help you place collages with respect to each other.
By keeping track of the dimensions,
the module can place collages next to each other, above each other,
align them to the left, to the top, etc.


## Main Design Goals

  - You can create _shapes_ like rectangles, circles, ellipses, polygons etc.
    By filling and/or outlining them, you turn them into a _collage_.

  - Something similar applies to _paths_.
    You can draw lines, line segments and paths.
    (Bezier curves are on the todo list!)
    By _tracing_ them, you turn them into a _collage_.

  - Other ways to create a collage are by including text, images or raw Html.

  - A _collage_ itself can than be transformed and grouped.
    Transformations include shifting, scaling, rotating, skewing, mirroring etc.
    By grouping collages they can be transformed as one.

  - Events can easily be added to a collage using the `Collage.Event` module.

  - You can laying out multiple collages in a compositional way by using the `Collage.Layout` module.
    This is similar to the functionality provided by the old `Graphics.Element` module
    and promoted by the popular Haskell [Diagrams library]().


### Summary

    Shape          Path         Text            Image       Html

    - polygon      - line       - fromString
    - ngon         - segment                      |           |
    - triangle     - path         |               |           |
    - rectangle                   |               |           |
    - square         |            |               |           |
    - oval           |            |               |           |
    - circle         |            |               |           |
      |              |            |               |           |
    filled         traced       rendered        included    embedded
    outlined         |            |               |           |
    styled           |            |               |           |
      |              |            |               |           |
      +––––––––––––––+––––––––––––+–––––––––––––––+–––––––––––+
                                  |
                                  |
                                  ˅

                               Collage  ˂––+
                                           |
                               - shift     |
                               - scale     |
                               - rotate    |
                               - opacity   |
                               - group     |
                                 |         |
                                 +–––––––––+


# Basics

@docs Point, opposite


# Shapes


## Drawing Shapes

_Rounded rectangles and squares are on the todo list..._

@docs Shape, polygon, ngon, triangle, rectangle, square, ellipse, circle


## Turning Shapes into Collages

@docs filled, outlined, styled


# Paths


## Drawing Paths

@docs Path, line, segment, path


## Turning Paths into Collages

@docs traced, loop


# Text


## Creating text

(See text module)


## Turning Text into Collages

@docs rendered


# Other Content

@docs included, embedded


# Collages

@docs Collage, BasicCollage, group


## Transforming Collages

@docs shift, scale, rotate, opacity


# Styling

There are three kind of styles:

  - Fill styles
  - Line styles
  - Text styles

Fill style
Text styles are defined in the Collage.Text module,
you can read all about them there.

@docs Style


## Fill Styles

For now we have only uniform fillings and a transparent filling.
Gradients and pattern fills are on the todo list.

@docs FillStyle, transparent


### Uniform fills

@docs uniform


### Gradient fills

_These are on the todo list..._


### Pattern fills

_These are on the todo list..._


## Line Styles

@docs LineStyle, invisible


### Line dashing

@docs solid, broken, dot, dash, longdash, dashdot


### Line thickness

We provide some sensible defaults for line thickness.
They are bluntly stolen from TikZ.

@docs ultrathin, verythin, thin, semithick, thick, verythick, ultrathick


### Line caps and joins

@docs LineCap, LineJoin

-}

import Collage.Core as Core
import Collage.Text as Text exposing (Text)
import Color exposing (Color)
import Html exposing (Html)


-- Basics ----------------------------------------------------------------------


{-| A 2-tuple of `Float`s representing a 2D point. `(0,0)` represents
a point in the center of the viewport.
-}
type alias Point =
    ( Float, Float )


{-| Calcualate the point at the opposite side of the origin.

Simply negates the coordinates:

    opposite ( x, y ) =
        ( -x, -y )

-}
opposite : Point -> Point
opposite ( x, y ) =
    ( -x, -y )



-- Collages -----------------------------------------------------------------------


{-| Anything that can be rendered on the screen. A `Collage` could be a
red circle, a line of text, or an arbitrary HTML element.

    redCircle : Collage
    redCircle =
        circle 10 |> solidFill (rgb 255 0 0) |> position ( -20, 0 )

    blueCircle : Collage
    blueCircle =
        circle 10 |> solidFill (rgb 0 0 255)

    circles : Collage
    circles =
        group [ redCircle, blueCircle ]

-}
type alias Collage msg =
    Core.Collage Core.FillStyle LineStyle Text.Style msg


{-| -}
type alias BasicCollage msg =
    Core.BasicCollage Core.FillStyle LineStyle Text.Style msg



-- Grouping Collages -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| Takes a list of `Collage`s and combines them into a single `Collage`.
-}
group : List (Collage msg) -> Collage msg
group =
    --FIXME: change renderer instead of using `List.reverse`. Svg draws last element in list on top!
    Core.collage << Core.Group << List.reverse



-- Transforming Collages -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
-- TODO:
-- * add scale in x and in y with nice names: widen/broaden and lengthen/stretch ???
-- * add skew in x and y with nice names: slant and tilt ???


{-| Shift a collage by the given amount (x,y) within its local space.

This is a relative translation,
so

    collage
        |> shift (5,10)

would shift `collage` five pixels to the right and ten pixels up.

Note that this influences the way collages are composed with the Collage.Layout module,
since collages are always composed with respect to their local origins.
Shifting a collage with `(5,10)` is equivalent to moving its local origin with `(-5,-10)`.

-}
shift : ( Float, Float ) -> Collage msg -> Collage msg
shift ( dx, dy ) collage =
    let
        ( x, y ) =
            collage.origin
    in
    { collage | origin = ( x + dx, y + dy ) }


{-| Scale a collage by a given factor.

Scaling by 2 doubles both dimensions and quadruples the area.

-}
scale : Float -> Collage msg -> Collage msg
scale s collage =
    { collage | scale = collage.scale * s }


{-| Rotate a collage by a given angle.

Rotate takes standard Elm angles,
which are **radians**,
and turns things **counterclockwise**.
So to turn `collage` 30&deg; to the left you would say:

    collage
        |> rotate (degrees 30)

-}
rotate : Float -> Collage msg -> Collage msg
rotate t collage =
    { collage | theta = collage.theta + t }


{-| Set the opacity of a collage.

The default is 1, and 0 is totally transparent.

-}
opacity : Float -> Collage msg -> Collage msg
opacity a collage =
    { collage | alpha = a }



-- Shapes ----------------------------------------------------------------------
-- TODO:
-- * add more primitive shapes: <circle>, <rect>


{-| Any kind of shape that can be filled and/or outlined.

Shapes only describe the dimensions of the figure.
Position, color, thickness, etc. are all specified later.

-}
type alias Shape =
    Core.Shape



-- Creating Shapes -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -


{-| Create an arbitrary polygon by specifying its corners in order.

`polygon` will automatically close all shapes, so the given list of points does not need to start and end with the same position.

-}
polygon : List Point -> Shape
polygon =
    Core.Polygon


{-| A regular polygon with _n_ sides.

The first argument specifies the number of sides and the second is the radius.

Some ngon's with radius 50:

    ngon 3 50  -- triangle
    ngon 5 50  -- pentagon
    ngon 8 50  -- octogon

-}
ngon : Int -> Float -> Shape
ngon n r =
    let
        m =
            toFloat n

        t =
            2 * pi / m

        f i =
            ( r * cos (t * toFloat i + pi / 2), r * sin (t * toFloat i + pi / 2) )
    in
    Core.Polygon <| List.map f (List.range 0 n)


{-| A triangle pointing upwards with given base.

Note the difference between using `triangle` and `ngon 3`.
Both produce a triangle pointing upwards with its origin in the center,
however:

  - `triangle base` gives us a triangle
    with three equal sides of length `base`
    and a distance from point to center of `sqrt 7 / 4 * base`.
  - `ngon 3 radius` gives us a similar triangle
    but with three equal sides of length `4 / sqrt 7 * radius`
    and a radius of `radius`.

-}
triangle : Float -> Shape
triangle b =
    let
        x =
            b / 2

        y =
            sqrt 3 / 2 * x
    in
    polygon [ ( -x, -y ), ( x, -y ), ( 0, y ) ]


{-| A rectangle of given width and height.
-}
rectangle : Float -> Float -> Shape
rectangle w h =
    let
        x =
            w / 2

        y =
            h / 2
    in
    polygon
        [ ( -x, -y )
        , ( x, -y )
        , ( x, y )
        , ( -x, y )
        ]


{-| A square of given size.

Off course this is equal to using `rectangle` with the same width and height:

    square size  ==  rectangle size size

-}
square : Float -> Shape
square n =
    rectangle n n



-- TODO: add roundedRect and roundedSquare


{-| An ellipse with given horizontal and vertical radii.

  - Note:
    the function `oval` in the original library acts a little bit different.
    It draws an oval of given width and height,
    so

    oval w h == ellipse (w/2) (h/2)

-}
ellipse : Float -> Float -> Shape
ellipse =
    Core.Ellipse


{-| A circle of given radius.

As with a square, using `circle` is the same as using `ellips` with the same x and y radii:

    circle radius  ==  ellipse radius radius

-}
circle : Float -> Shape
circle r =
    ellipse r r



-- Turning Shapes into Collages -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| Adds a fills to a shape, turining it into a collage.

The argument specifies the style of the fill.
The **outline is left invisible**.
To draw a red circle of radius 50 you say:

    circle 50
        |> filled (uniform red)

See below for possible fill styles.

-}
filled : FillStyle -> Shape -> Collage msg
filled fill =
    styled ( fill, invisible )


{-| Adds an outline to a shape, turning it into a collage.

The arguments specify the style of the outline.
The **fill is left transparent**.
To draw a square with edge length 30 with a thin black dashed outline you say:

    square 30
        |> outlined (dot thin (uniform black))

See below for more possible line styles.

-}
outlined : LineStyle -> Shape -> Collage msg
outlined line =
    styled ( transparent, line )


{-| Adds a fill and an outline to a shape, turning it into a collage.

The tuple argument contains fill style and a line style.
To draw an thick black outlined green triangle with base 30 you say:

    triangle 30
        |> styled (uniform green, solid thick (uniform black))

The tuple form helps in defining your own reusable styles.
For example, if you like all you shapes to have a thick black outline,
you could rewrite above example to:

    thickOutlinedAndFilled fillColor =
        (uniform fillColor, solid thick (uniform black))

    triangle 30
        |> styled (thickOutlinedAndFilled green)

See below for all possible fill and line styles.

-}
styled : ( FillStyle, LineStyle ) -> Shape -> Collage msg
styled style =
    Core.collage << Core.Shape style



-- Paths -----------------------------------------------------------------------
-- TODO:
-- * add more primitive paths: <line>, <path>


{-| A segment of a line or curve. Only describes the shape of the line.
Position, color, thickness, etc. are all specified later.
-}
type alias Path =
    Core.Path



-- Creating Paths -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- TODO: add curves (aka Bezier paths), arcs (part of Bezier paths)
-- TODO: add way to close a path so it can be filled?
--       something like `close : Path -> Shape`


{-| | Draw a horizontal line with a given length.

The origin of the line will be `(0,0)`.

-}
line : Float -> Path
line l =
    path [ ( -l / 2, 0 ), ( l / 2, 0 ) ]


{-| `segment (x1,y1) (x2,y2)` is a line segment with
endpoints at `(x1,y1)` and `(x2,y2)`.
-}
segment : Point -> Point -> Path
segment a b =
    path [ a, b ]


{-| `polyline points` is a polyline with vertices
at `points`. (A polyline is a collection of connected
line segments. It can be thought of as drawing a
"connect-the-dots" line through a list of points.)
-}
path : List Point -> Path
path =
    Core.Polyline



-- Turning Paths into Collages -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
{-
   -- ORIG
   segment (0,0) (1,1)
       |> traced (dashed red)
   rectangle 4 5
       |> filled red
       AND
       |> outlined (solid red)

   -- OLD
   segment (0,0) (1,1)
        |> dashed 2 (solid red)
   -- NEW
   segment (0,0) (1,1)
       |> traced (dash 2) (uniform red)
       OR
       |> traced (dash 2 (uniform red))
       OR
       |> dashed 2 (uniform red)
   rectangle 4 5
       |> filled (uniform red)
       AND
       |> outlined (solid 1) (uniform red)
       OR
       |> outlined (solid 1 (uniform red))

-}


{-| Trace a path with a given line style.
-}
traced : LineStyle -> Path -> Collage msg
traced style path =
    Core.collage <| Core.Path style path


{-| Close a path so that it can be outlined and filled.
-}
loop : Path -> Shape
loop =
    Core.Loop



-- Text ------------------------------------------------------------------------


{-| -}
rendered : Text -> Collage msg
rendered text =
    Core.collage <| Core.Text ( Text.width text, Text.height text ) text



-- Images ----------------------------------------------------------------------


{-| An image. The arguments specify the image's thickness, height and url.
-}
included : ( Float, Float ) -> String -> Collage msg
included dims =
    Core.collage << Core.Image dims



-- Raw Content -----------------------------------------------------------------


{-| Creates a `Collage` from an arbitrary `Html` element. The
resulting collage is subject to all of the regular manipulations.
Note that if you are compiling to SVG, then this functionality
is not supported in Internet Explorer.
-}
embedded : ( Float, Float ) -> Html msg -> Collage msg
embedded dims =
    Core.collage << Core.Element dims



-- Styling ---------------------------------------------------------------------


{-| Convenience shorthand for styling.
FIXME: good idea???
-}
type alias Style =
    ( FillStyle, LineStyle )



-- Fill Styles -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| Describes the texture of a shape or line. It can be a uniform color,
gradient, or tiled texture.
-}
type alias FillStyle =
    Core.FillStyle


{-| Uniform color fill
-}
uniform : Color -> FillStyle
uniform =
    Core.Uniform


{-| Transparent texture
-}
transparent : FillStyle
transparent =
    Core.Transparent



-- Line Styles -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| Speficies the styling (color, thickness, dashing, etc.) of a line.

    -- defines a red, dashed line with a thickness of 5px
    { color = rgb 255 20 20
    , thickness = 5
    , cap = Flat
    , join = Sharp
    , dashing = [8,4]
    , dashOffset = 0
    }

(Record is not shown due to technical limitations of interal exports in Elm.)

-}
type alias LineStyle =
    { fill : FillStyle
    , thickness : Float
    , cap : LineCap
    , join : LineJoin
    , dashPattern : List ( Int, Int )
    , dashPhase : Int
    }


{-| Invisible line
-}
invisible : LineStyle
invisible =
    solid 0 transparent


{-| Creates a Collage representing a solid line from a
'Path' object. The first argument specifies the line
thickness and the second argument specifies the texture
to use for the line stroke.
-}
solid : Float -> FillStyle -> LineStyle
solid =
    broken []


{-| A custom line defined by a list of (on,off):
broken [(10,5)] 5 -- a line that with dashes 10 long and spaces 5 long
broken [(10,5),(20,5)] -- on for 10, off 5, on 20, off 5
-}
broken : List ( Int, Int ) -> Float -> FillStyle -> LineStyle
broken dash thickness fill =
    { fill = fill
    , thickness = thickness
    , cap = Flat
    , join = Sharp
    , dashPattern = dash
    , dashPhase = 0
    }



--FIXME: good idea to calculate lenght based on thickness?


{-| The same as `solid`, except the line is dots.

Calulates the length of the dots based on the line thickness.

-}
dot : Float -> FillStyle -> LineStyle
dot thickness =
    let
        d =
            round thickness
    in
    broken [ ( d, d ) ] thickness


{-| The same as `solid`, except the line is dashed.
-}
dash : Float -> FillStyle -> LineStyle
dash thickness =
    let
        d =
            round thickness
    in
    broken [ ( d * 5, d * 2 ) ] thickness


{-| Define a dashed line type with the given thickness, where the dashes are longer than normal.
-}
longdash : Float -> FillStyle -> LineStyle
longdash thickness =
    let
        d =
            round thickness
    in
    broken [ ( d * 12, d * 6 ) ] thickness


{-| Define a line type with the given thickness, including alternating dots and dashes.
-}
dashdot : Float -> FillStyle -> LineStyle
dashdot thickness =
    let
        d =
            round thickness
    in
    broken [ ( d * 5, d ), ( d, d ) ] thickness



-- Line Thickness -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| 0.5 px
-}
ultrathin : Float
ultrathin =
    0.5


{-| 1 px
-}
verythin : Float
verythin =
    1.0


{-| 2 px
-}
thin : Float
thin =
    2.0


{-| 3 px
-}
semithick : Float
semithick =
    3.0


{-| 4 px
-}
thick : Float
thick =
    4.0


{-| 6 px
-}
verythick : Float
verythick =
    6.0


{-| 8 px
-}
ultrathick : Float
ultrathick =
    8.0



-- Line Caps and Joins -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| Describes the cap style of a line. `Flat` capped lines have
no endings, `Padded` capped lines have flat endings that extend
slightly past the end of the line, and `Round` capped lines have
hemispherical endings.

In TikZ and Css these options are called:

    = Butt
    | Round
    | Rect

-}
type LineCap
    = Flat
    | Round
    | Padded


{-| Describes the join style of a line.

In TikZ and Css these options are called:

    = Round
    | Bevel
    | Miter

-}
type LineJoin
    = Smooth
    | Clipped
    | Sharp
