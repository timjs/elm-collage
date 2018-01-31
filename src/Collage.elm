module Collage
    exposing
        ( Collage
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
        , close
        , dash
        , dashdot
        , defaultLineStyle
        , dot
        , ellipse
        , filled
        , group
        , html
        , image
        , invisible
        , line
        , longdash
        , ngon
        , opacity
        , opposite
        , outlined
        , path
        , polygon
        , rectangle
        , rendered
        , rotate
        , roundedRectangle
        , roundedSquare
        , scale
        , scaleX
        , scaleY
        , segment
        , semithick
        , shift
        , shiftX
        , shiftY
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
You can style all sorts of forms including shapes, paths, text, and images,
and then shift, rotate, scale, and group them.


### Contents

  - [Basics](#basics)
      - [Transforming collages](#transforming-collages)
      - [Grouping collages](#grouping-collages)
  - [Shapes](#shapes)
      - [Drawing shapes](#drawing-shapes)
      - [Turning shapes into collages](#turning-shapes-into-collages)
  - [Paths](#paths)
      - [Drawing paths](#drawing-paths)
      - [Turning paths into collages or shapes](#turning-paths-into-collages-or-shapes)
  - [Text](#text)
  - [Images and Html](#images-and-html)
  - [Styling](#styling)
      - [Fill styles](#fill-styles)
      - [Line styles](#line-styles)
          - [Line dashing](#line-dashing)
          - [Line thickness](#line-thickness)
          - [Line caps and joins](#line-caps-and-joins)


### Coordinate system

Collages use the same coordinate system you might see in an algebra or physics problem.
The origin (0,0) is **at the center** of the collage, not the top left corner as in some other graphics libraries.
Furthermore, the **y-axis points up**: so moving a collage 10 units in the y-axis will move it up on screen.
This is intentional.
The goal is to provide an elegant interface which is abstracted as much as possible from implementation details.


### Creating graphics

To create a graphic you start with creating some form: a _shape_, a _path_ or a chunk of _text_.
After creating a form, you can _style_ it.
It depends on the kind of form you created how you can style it.
A shape, for example, can be filled with a uniform color,
outlined with a dotted line,
or both.
A path only can be traced with a line style
and a piece of text can be made monospaced, bold, italic, underlined etc.
You can think of a form as some kind of _stencil_,
dipping it in different colors of ink and stamp it onto the screen once or multiple times.

Styling a form will turn it into a _collage_.
Collages are the most powerful object of this library.
They cannot be styled anymore,
but they can be shifted, rotated, scaled, made partially transparent, and grouped into a new collage.
Yeah, you read that correctly:
you can group multiple collages into a new one,
which you can shift, rotate, scale, and group again!

So remember:
before you can transform your drawing,
you have to style it (i.e. turn it into a _collage_),
only after styling you can shift, rotate, scale, etc.
Including an external image or a piece of raw Html also belongs to the possibilities.
And of course they can be shifted, rotated, scaled, ...
Ok, you get the grip!


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
    filled         traced       rendered        image       html
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

@docs Point, opposite, Collage


## Transforming collages

@docs shift, shiftX, shiftY, scale, scaleX, scaleY, rotate, opacity


## Grouping collages

@docs group


# Shapes


## Drawing shapes

@docs Shape, rectangle, square, roundedRectangle, roundedSquare, ellipse, circle, polygon, ngon, triangle


## Turning shapes into collages

@docs filled, outlined, styled


# Paths

_Please fill in an issue if you want support for curves and arcs (aka Bézier paths).
I like to know if people want this before implementing it._


## Drawing paths

@docs Path, line, segment, path


## Turning paths into collages or shapes

@docs traced, close


# Text

To create and style text, take a look at the Collage.Text module.

@docs rendered


# Images and Html

@docs image, html


# Styling

There are three kinds of styles:

  - Fill styles
  - Line styles
  - Text styles

Fill styles are used to fill a shape
or color a line.
Therefore, line styles contain a fill style.
Line styles can be used to trace paths
or outline shapes.
Text styles are defined in the Collage.Text module,
you can read all about them there.

@docs Style


## Fill styles

For now, we have only uniform fillings and a transparent filling.

_Please fill in an issue if you want support for gradients and patterns.
I like to know if people want this before implementing it._

@docs FillStyle, transparent, uniform


## Line styles

@docs LineStyle, invisible, defaultLineStyle


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
a point in the center of the canvas.
-}
type alias Point =
    ( Float, Float )


{-| Calculate the point at the opposite side of the origin.

Simply negates the coordinates:

    opposite ( x, y ) =
        ( -x, -y )

-}
opposite : Point -> Point
opposite ( x, y ) =
    ( -x, -y )



-- Collages -----------------------------------------------------------------------


{-| An opaque type representing any styled form or group of forms that can be shifted, rotated, scaled, etc.
A collage could be a red circle, a dotted line, a chunk of text, or an arbitrary Html element.
-}
type alias Collage msg =
    Core.Collage Core.FillStyle LineStyle Text.Style msg



-- Grouping collages -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| Take a list of collages and combine them into a single collage,
which again can be shifted, rotated, scaled, etc.

    group [drawing1, drawing2, drawing3]
        |> scale 3
        |> rotate (degrees 90)

-}
group : List (Collage msg) -> Collage msg
group =
    Core.collage << Core.Group



-- Transforming collages -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
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
            collage.shift
    in
    { collage | shift = ( x + dx, y + dy ) }

{-| Shift a collage by the given amount on the X axis within its local space.

-}
shiftX : Float -> Collage msg -> Collage msg
shiftX dx collage =
    let
        ( x, y ) =
            collage.shift
    in
    { collage | shift = ( x + dx, y ) }

{-| Shift a collage by the given amount on the Y axis within its local space.

-}
shiftY : Float -> Collage msg -> Collage msg
shiftY dy collage =
    let
        ( x, y ) =
            collage.shift
    in
    { collage | shift = ( x, y + dy ) }


{-| Scale a collage by a given factor.

Scaling by 2 doubles both dimensions and quadruples the area.

-}
scale : Float -> Collage msg -> Collage msg
scale s collage =
    scaleXY ( s, s ) collage


{-| Scale a collage horizontally (in its local space) by a given factor.

Scaling by 2 doubles the width and doubles the area.

-}
scaleX : Float -> Collage msg -> Collage msg
scaleX s collage =
    scaleXY ( s, 1 ) collage


{-| Scale a collage vertically (in its local space) by a given factor.

Scaling by 2 doubles the height and doubles the area.

-}
scaleY : Float -> Collage msg -> Collage msg
scaleY s collage =
    scaleXY ( 1, s ) collage


scaleXY : ( Float, Float ) -> Collage msg -> Collage msg
scaleXY ( sx, sy ) collage =
    let
        ( sx0, sy0 ) =
            collage.scale
    in
    { collage | scale = ( sx0 * sx, sy0 * sy ) }


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
    { collage | rotation = collage.rotation + t }


{-| Set the opacity of a collage.

The default is 1, and 0 is totally transparent.

-}
opacity : Float -> Collage msg -> Collage msg
opacity a collage =
    { collage | opacity = a }



-- Shapes ----------------------------------------------------------------------


{-| Any kind of shape that can be filled and/or outlined.

Shapes only describe the dimensions of the figure.
Position, color, thickness, etc. are all specified later.

-}
type alias Shape =
    Core.Shape



-- Creating shapes -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -


{-| Create an arbitrary polygon by specifying its corners in order.

`polygon` will automatically close all shapes,
so the given list of points does not need to start and end with the same position.

  - Note:
    Be sure the origin of your polygon is at the right position if you like to automatically position collages.
    E.g. use `center` or `align topLeft` from the Collage.Layout module to fix this.

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
    polygon <| List.map f (List.range 0 n)


{-| An equilateral triangle pointing upwards with given base.

Note the difference between using `triangle` and `ngon 3`.
Both produce a triangle pointing upwards with its origin in the center,
however:

  - `triangle base` gives us a triangle
    with three equal sides of length `base`
    and a distance from each corner to the center of `sqrt 7 / 4 * base`.
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
    roundedRectangle w h 0


{-| A square of given size.

Of course this is equal to using `rectangle` with the same width and height:

    square size  ==  rectangle size size

-}
square : Float -> Shape
square size =
    rectangle size size


{-| A rectangle with rounded corners.

First two arguments are for the width and height of the rectangle,
last is the radius of the corners.

-}
roundedRectangle : Float -> Float -> Float -> Shape
roundedRectangle =
    Core.Rectangle


{-| A square with rounded corners.

Of course this is equal to using `roundedRectangle` with the same width and height:

    roundedSquare size  ==  roundedRectangle size size

-}
roundedSquare : Float -> Float -> Shape
roundedSquare size =
    roundedRectangle size size


{-| An ellipse with given horizontal and vertical radii.

  - Note:
    the function `oval` in the original library acts a little bit different.
    It draws an oval of given width and height,
    so

        oval w h  ==  ellipse (w/2) (h/2)

-}
ellipse : Float -> Float -> Shape
ellipse =
    Core.Ellipse


{-| A circle of given radius.

As with a square, using `circle` is the same as using `ellipse` with the same x and y radii:

    circle radius  ==  ellipse radius radius

-}
circle : Float -> Shape
circle =
    Core.Circle



-- Turning shapes into collages -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| Adds a fills to a shape, turning it into a collage.

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

See below for the possible line styles.

-}
outlined : LineStyle -> Shape -> Collage msg
outlined line =
    styled ( transparent, line )


{-| Adds a fill and an outline to a shape, turning it into a collage.

The tuple argument contains a fill style and a line style.
To draw a thick black outlined green triangle with base 30 you say:

    triangle 30
        |> styled
            ( uniform green
            , solid thick (uniform black)
            )

The tuple form helps in defining your own reusable styles.
For example, if you want more of you shapes to have a thick black outline,
you could rewrite above example to:

    thickOutlinedAndFilled fillColor =
        ( uniform fillColor, solid thick (uniform black) )

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


{-| A 2D line or curve that can be traced.

Paths only describe the shape of the line.
Position, color, thickness, etc. are all specified later.
Paths can **only be traced** by a line style,
not filled.
If you like to fill a path,
you have to _close_ it.
This will turn a path it into a shape,
which can be filled and outlined.

-}
type alias Path =
    Core.Path



-- Creating paths -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- TODO: add curves (aka Bezier paths), arcs (part of Bezier paths)


{-| Draw a horizontal line with a given length.

The origin of the line will be `(0,0)`.
Here is a thick dotted yellow horizontal line of length 20:

    line 20
        |> traced (dot thick (uniform yellow))

-}
line : Float -> Path
line l =
    path [ ( -l / 2, 0 ), ( l / 2, 0 ) ]


{-| Create a path along a given line segment.
Takes the start and end points of the segment as arguments.

To draw a sloped blue line from (0,5) to (5,0) you say:

    segment ( 0, 5 ) ( 5, 0 )
        |> traced (uniform blue)

  - Note:
    If you like to automatically position lines,
    be sure the origin is at the right position.
    E.g. use `center` or `align topLeft` from Collage.Layout.
    Most of the time using the `line` function above and rotating or shifting it makes things more clear.

-}
segment : Point -> Point -> Path
segment a b =
    path [ a, b ]


{-| Create a path that follows a sequence of points.

It can be thought of as drawing a “connect-the-dots” line through a list of points.

-}
path : List Point -> Path
path =
    Core.Polyline



-- Turning paths into collages -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
{-
   Some possibilities for an API:

      segment (0,0) (1,1)
          -- let traced take a dash patter + thickness and a fill color:
          |> traced (dash 2) (uniform red)
          -- let traced take a style:
          |> traced (dash 2 (uniform red))
          -- use solid, dashed, dotted etc to turn something into a collage (Elm Render):
          |> dashed 2 (uniform red)
          -- ommit the thickness (Elm Graphics):
          |> traced (dashed red)

      rectangle 4 5
          |> filled (uniform red)
          OR
          |> filled red

      rectangle 4 5
          -- when using a style, we can reuse the functions of tracing:
          |> outlined (solid 1 (uniform red))
          -- otherwise we need to create a type for dash patterns (solid 1):
          |> outlined (solid 1) (uniform red)
          -- just the dash pattern and a color (Elm Graphics):
          |> outlined (solid red)

-}


{-| Trace a path with a given line style.

Here is a red zig-zag:

    path [( 0, 5 ), ( 5, 0 ), ( 5, 5 )]
        |> traced (solid thin (uniform red))

Paths can only be traced.
If you like to fill a path,
you have to turn it into a shape by _closing_ it first.

-}
traced : LineStyle -> Path -> Collage msg
traced style path =
    Core.collage <| Core.Path style path


{-| Close a path so that it also can be filled.

**Note:**
Does not draw a line from start to end point for you.
If you really want this, you have two options:

1.  Draw it yourself
2.  Use a polygon

-}
close : Path -> Shape
close =
    Core.Loop



-- Text ------------------------------------------------------------------------


{-| Render a chunk of styled text and turn it into a collage.

    Text.fromString "Hello Collage!"
        |> Text.shape Text.Italic
        |> Text.size huge
        |> rendered

See the Collage.Text module for all the possibilities to create and style text.

-}
rendered : Text -> Collage msg
rendered text =
    Core.collage <| Core.Text ( Text.width text, Text.height text ) text



-- Raw Content -----------------------------------------------------------------


{-| Create an image given a width, height, and image source.

    image 100 100 "elm-logo.jpg"

-}
image : ( Float, Float ) -> String -> Collage msg
image dims =
    Core.collage << Core.Image dims


{-| Create a collage from an arbitrary Html element.

The resulting collage is subject to all of the regular transformations.

-}
html : ( Float, Float ) -> Html msg -> Collage msg
html dims =
    Core.collage << Core.Html dims



-- Styling ---------------------------------------------------------------------


{-| Convenience shorthand for styling.
-}
type alias Style =
    ( FillStyle, LineStyle )



-- Fill Styles -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| Describes the fill of a shape or line.

For now, it can only be a uniform color or no fill at all.

-}
type alias FillStyle =
    Core.FillStyle


{-| Uniform color fill.
-}
uniform : Color -> FillStyle
uniform =
    Core.Uniform


{-| Transparent fill.
-}
transparent : FillStyle
transparent =
    Core.Transparent



-- Line Styles -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| All of the attributes of a line style.

This lets you build up a line style however you want.
You can also update existing line styles with record updates.

To define a red, dashed line style with a thickness of 5px:

    { color = rgb 255 20 20
    , thickness = 5
    , cap = Flat
    , join = Sharp
    , dashing = [8,4]
    , dashOffset = 0
    }

-}
type alias LineStyle =
    { fill : FillStyle
    , thickness : Float
    , cap : LineCap
    , join : LineJoin
    , dashPattern : List ( Int, Int )
    , dashPhase : Int
    }


{-| The default line style, which is solid black with flat caps and sharp joints.

You can use record updates to build the line style you want.
For example, to make a thicker line, you could say:

    { defaultLineStyle | width = verythick }

-}
defaultLineStyle : LineStyle
defaultLineStyle =
    { fill = uniform Color.black
    , thickness = thin
    , cap = Flat
    , join = Sharp
    , dashPattern = []
    , dashPhase = 0
    }


{-| Invisible line.
-}
invisible : LineStyle
invisible =
    solid 0 transparent


{-| A line style representing a solid line of given thickness and color.
-}
solid : Float -> FillStyle -> LineStyle
solid =
    broken []


{-| A custom line defined by a list of `(on, off)` dash length:

    broken [(10,5)]         -- a line that with dashes 10 long and spaces 5 long
    broken [(10,5),(20,5)]  -- on for 10, off 5, on 20, off 5

-}
broken : List ( Int, Int ) -> Float -> FillStyle -> LineStyle
broken dash thickness fill =
    { defaultLineStyle
        | fill = fill
        , thickness = thickness
        , dashPattern = dash
    }


{-| A dotted line type with the given thickness.

Calculates the length of the dots based on the given line thickness.

-}
dot : Float -> FillStyle -> LineStyle
dot thickness =
    let
        d =
            round thickness
    in
    broken [ ( d, d ) ] thickness


{-| A dashed line type with the given thickness.

Calculates the length of the dashes based on the given line thickness.

-}
dash : Float -> FillStyle -> LineStyle
dash thickness =
    let
        d =
            round thickness
    in
    broken [ ( d * 5, d * 2 ) ] thickness


{-| A dashed line type with the given thickness, where the dashes are longer than normal.

Calculates the length of the dashes based on the given line thickness.

-}
longdash : Float -> FillStyle -> LineStyle
longdash thickness =
    let
        d =
            round thickness
    in
    broken [ ( d * 12, d * 6 ) ] thickness


{-| A dashed line type with the given thickness, including alternating dots and dashes.

Calculates the length of the dashes based on the given line thickness.

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


{-| The shape of the end of a line.

`Flat` capped lines have no endings,
`Padded` capped lines have flat endings that extend slightly past the end of the line,
and `Round` capped lines have hemispherical endings.

In TikZ and Css these options are called butt, rect, and round.

-}
type LineCap
    = Flat
    | Round
    | Padded



--TODO: Sharp takes an argument to limit the length of the joint. This defaults to 10.


{-| The shape of the “joints” of a line, where each line segment meets.

In TikZ and Css these options have the nondescriptive names round, miter, and bevel.

-}
type LineJoin
    = Smooth
    | Sharp
    | Clipped
