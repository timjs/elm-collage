module Collage
    exposing
        ( Point
        , Collage
        , BasicCollage
        , group
        , translate
        , scale
        , rotate
        , opacity
        , Shape
        , polygon
        , ngon
        , triangle
        , rectangle
        , square
        , ellipse
        , circle
        , filled
        , outlined
        , styled
        , Path
        , segment
        , path
        , traced
        , left
        , centered
        , right
        , image
        , html
        , FillStyle
        , uniform
        , transparent
        , LineStyle
        , solid
        , invisible
        , dot
        , dash
        , longdash
        , dotdash
        , broken
        , LineCap
        , LineJoin
        )

{-| This library provides a toolkit for rendering and manipulating
graphics primitives such as lines, polygons, text, images, etc.
It is intended primarily for projects that are too complex for
the manual manipulation of an SVG or HTML5 canvas element, but too
simple for a fully blown graphics engine such as WebGL (a motivating
example would be a simple 2D game).

In theory, the structure of this library allows for multiple easily
interchangable backend rendering targets (i.e. SVG, HTML5 canvas), but
the only backend supported at present is SVG.


# Basics

@docs Point


# Collages

@docs Collage, BasicCollage, group


## Manipulating Collages

@docs translate, scale, rotate, opacity


# Shapes

@docs Shape, polygon, ngon, triangle, rectangle, square, ellipse, circle


## Turning Shapes into Collages

@docs filled, outlined, styled


# Paths

@docs Path, segment, path


## Turning Paths into Collages

@docs traced


# Other Content

@docs left, centered, right, image, html


# Styling


## Textures

@docs FillStyle, uniform, transparent


## Strokes

@docs LineStyle, solid, invisible, dot, dash, longdash, dotdash, broken, LineCap, LineJoin

-}

import Html exposing (Html)
import Color exposing (Color)
import Text exposing (Text)
import Collage.Core as Core


-- Basics ----------------------------------------------------------------------


{-| A 2-tuple of `Float`s representing a 2D point. `(0,0)` represents
a point in the center of the viewport.
-}
type alias Point =
    ( Float, Float )



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
    Core.Collage msg


{-| Basic form type. Public to support multiple rendering enginges.
-}
type alias BasicCollage msg =
    Core.BasicCollage msg



-- Creating Collages -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


form : BasicCollage msg -> Collage msg
form basic =
    Core.Collage ( 0, 0 ) 0 1 1 basic []



-- Grouping Collages -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| Takes a list of `Collage`s and combines them into a single
`Collage`.
-}
group : List (Collage msg) -> Collage msg
group forms =
    form <| Core.Group forms



-- Transforming Collages -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
-- TODO:
-- * add scale in x and in y with nice names: widen/broaden and lengthen/stretch ???
-- * add skew in x and y with nice names: slant and tilt ???


{-| Move a form by the given amount (x, y). This is a relative translation so
`(move (5,10) form)` would move `form` five pixels to the right and ten pixels up.
-}
translate : ( Float, Float ) -> Collage msg -> Collage msg
translate ( tx, ty ) form =
    let
        ( x, y ) =
            form.origin
    in
        { form | origin = ( x + tx, y + ty ) }


{-| Scale a form by a given factor. Scaling by 2 doubles both dimensions,
and quadruples the area.
-}
scale : Float -> Collage msg -> Collage msg
scale s form =
    { form | scale = form.scale * s }


{-| Rotate a form by a given angle. Rotate takes standard Elm angles (radians)
and turns things counterclockwise. So to turn `form` 30&deg; to the left
you would say, `(rotate (degrees 30) form)`.
-}
rotate : Float -> Collage msg -> Collage msg
rotate t form =
    { form | theta = form.theta + t }


{-| Set the alpha of a `Collage msg`. The default is 1, and 0 is totally transparent.
-}
opacity : Float -> Collage msg -> Collage msg
opacity a form =
    { form | alpha = a }



-- Shapes ----------------------------------------------------------------------
-- TODO:
-- * add more primitive shapes: <circle>, <rect>


{-| A polygon or an ellipse. Only describes the size and shape of the figure.
Position, color, thickness, etc. are all specified later.
-}
type alias Shape =
    Core.Shape



-- Creating Shapes -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -


{-| `polygon points` is a polygon bounded by `points`.
-}
polygon : List Point -> Shape
polygon =
    Core.Polygon


{-| Create a regular polygon with a given number of sides and radius.

Examples:

    ngon 3 50 - triangle
    ngon 5 50 - pentagon
    ngon 8 50 - octogon

-}
ngon : Int -> Float -> Shape
ngon n r =
    let
        m =
            toFloat n

        t =
            2 * pi / m

        f i =
            ( r * cos (t * toFloat i), r * sin (t * toFloat i) )
    in
        Core.Polygon <| List.map f (List.range 0 n)


{-| -}
triangle : Float -> Shape
triangle =
    ngon 3


{-| A rectangle. The arguments specify thickness and height, respectively.
-}
rectangle : Float -> Float -> Shape
rectangle w h =
    let
        halfW =
            w / 2

        halfH =
            h / 2
    in
        polygon
            [ ( 0 - halfW, halfH )
            , ( halfW, halfH )
            , ( halfW, 0 - halfH )
            , ( 0 - halfW, 0 - halfH )
            ]


{-| A square with a given edge length.
-}
square : Float -> Shape
square n =
    rectangle n n



-- TODO: add roundedRect and roundedSquare


{-| An ellipse. The arugments specify the horizontal and vertical radii,
respectively.
NOTE: called `oval` in original lib
-}
ellipse : Float -> Float -> Shape
ellipse =
    Core.Ellipse


{-| A circle. The argument specifies the radius.
-}
circle : Float -> Shape
circle r =
    ellipse r r



-- Turning Shapes into Collages -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| Fills in a shape, making it into a 'Collage'. The argument
specifies the texture of the fill. The line is left transparent.
-}
filled : FillStyle -> Shape -> Collage msg
filled style =
    styled style invisible


{-| Adds a line to a shape, making it into a 'Collage'. The arguments
specify the thickness and texture of the line, respectiverly. The fill is
left transparent.
-}
outlined : LineStyle -> Shape -> Collage msg
outlined style =
    styled transparent style


{-| Adds a fill and line to a 'Shape', making it into a 'Collage'. The
first argument specifies the fill texture, and the second two arguments
specify the line thickness and texture, respectively.
-}
styled : FillStyle -> LineStyle -> Shape -> Collage msg
styled texture stroke shape =
    form <| Core.Shape { fill = texture, line = stroke } shape



-- Paths -----------------------------------------------------------------------
-- TODO:
-- * add more primitive paths: <line>, <path>


{-| A segment of a line or curve. Only describes the shape of the line.
Position, color, thickness, etc. are all specified later.
-}
type alias Path =
    Core.Path



-- Creating Paths -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| `polyline points` is a polyline with vertices
at `points`. (A polyline is a collection of connected
line segments. It can be thought of as drawing a
"connect-the-dots" line through a list of points.)
-}
path : List Point -> Path
path =
    Core.Polyline


{-| `segment (x1,y1) (x2,y2)` is a line segment with
endpoints at `(x1,y1)` and `(x2,y2)`.
FIXME: rename to `line`?
-}
segment : Point -> Point -> Path
segment a b =
    path [ a, b ]



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
    form <| Core.Path style path



-- Text ------------------------------------------------------------------------


{-| -}
left : Text -> Collage msg
left =
    form << Core.Text Core.Start


{-| -}
centered : Text -> Collage msg
centered =
    form << Core.Text Core.Middle


{-| -}
right : Text -> Collage msg
right =
    form << Core.Text Core.End



-- Images ----------------------------------------------------------------------


{-| An image. The arguments specify the image's thickness, height and url.
-}
image : Float -> Float -> String -> Collage msg
image w h url =
    form <| Core.Image url w h



-- Raw Content -----------------------------------------------------------------


{-| Creates a `Collage` from an arbitrary `Html` element. The
resulting form is subject to all of the regular manipulations.
Note that if you are compiling to SVG, then this functionality
is not supported in Internet Explorer.
-}
html : Html msg -> Collage msg
html elem =
    form <| Core.Element elem



-- Styling ---------------------------------------------------------------------
-- Fill Styles -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| Describes the texture of a shape or line. It can be a uniform color,
gradient, or tiled texture.
-}
type alias FillStyle =
    Core.FillStyle



-- | Gradient Gradient
-- | Pattern Float Float String Float
-- | FillStyle String


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

-}
type alias LineStyle =
    Core.LineStyle


{-| Creates a Collage representing a solid line from a
'Path' object. The first argument specifies the line
thickness and the second argument specifies the texture
to use for the line stroke.
-}
solid : Float -> FillStyle -> LineStyle
solid =
    broken []


{-| Invisible line
-}
invisible : LineStyle
invisible =
    solid 0 transparent


{-| The same as `solid`, except the line is dots.
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
dotdash : Float -> FillStyle -> LineStyle
dotdash thickness =
    let
        d =
            round thickness
    in
        broken [ ( d, d ), ( d * 5, d ) ] thickness


{-| A custom line defined by a list of (on,off):
broken [(10,5)] 5 -- a line that with dashes 10 long and spaces 5 long
broken [(10,5),(20,5)] -- on for 10, off 5, on 20, off 5
-}
broken : List ( Int, Int ) -> Float -> FillStyle -> LineStyle
broken dash thickness texture =
    Core.LineStyle texture thickness Core.Flat Core.Sharp dash 0


{-| Describes the cap style of a line. `Flat` capped lines have
no endings, `Padded` capped lines have flat endings that extend
slightly past the end of the line, and `Round` capped lines have
hemispherical endings.
-}
type alias LineCap =
    Core.LineCap


{-| Describes the join style of a line.
-}
type alias LineJoin =
    Core.LineJoin
