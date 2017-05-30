module Collage
    exposing
        ( Point
        , Form
        , BasicForm(..)
        , group
        , move
        , moveX
        , moveY
        , scale
        , rotate
        , alpha
        , Shape(..)
        , polygon
        , ngon
        , rect
        , square
        , ellipse
        , circle
        , Style
        , filled
        , stroked
        , filledAndStroked
        , styled
        , Path(..)
        , segment
        , path
        , traced
        , image
        , html
        , Texture(..)
        , uniform
        , transparent
        , Stroke
        , solid
        , dot
        , dash
        , longdash
        , dotdash
        , broken
        , LineCap(..)
        , LineJoin(..)
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


# Forms

@docs Form, BasicForm, group


## Manipulating Forms

@docs move, moveX, moveY, scale, rotate, alpha


# Shapes

@docs Shape, polygon, ngon, rect, square, ellipse, circle


## Turning Shapes into Forms

@docs Style, filled, stroked, filledAndStroked, styled


# Paths

@docs Path, segment, path


## Turning Paths into Forms

@docs traced


# Images

@docs image


# Raw Content

@docs html


# Styling


## Textures

@docs Texture, uniform, transparent


## Strokes

@docs Stroke, solid, dot, dash, longdash, dotdash, broken, LineCap, LineJoin

-}

import Html exposing (Html)
import Color exposing (Color, Gradient)
import Json.Decode as Json


-- Basics ----------------------------------------------------------------------


{-| A 2-tuple of `Float`s representing a 2D point. `(0,0)` represents
a point in the center of the viewport.
-}
type alias Point =
    ( Float, Float )



-- Forms -----------------------------------------------------------------------


{-| Anything that can be rendered on the screen. A `Form` could be a
red circle, a line of text, or an arbitrary HTML element.

    redCircle : Form
    redCircle =
        circle 10 |> solidFill (rgb 255 0 0) |> position ( -20, 0 )

    blueCircle : Form
    blueCircle =
        circle 10 |> solidFill (rgb 0 0 255)

    circles : Form
    circles =
        group [ redCircle, blueCircle ]

-}
type alias Form msg =
    { x : Float
    , y : Float
    , theta : Float
    , scale : Float
    , alpha : Float
    , form : BasicForm msg
    , handlers : List ( String, Json.Decoder msg )
    }


{-| Basic form type. Public to support multiple rendering enginges.
-}
type BasicForm msg
    = Shape Shape Style
    | Path Path Stroke
      -- | Text Text TextAlign
    | Image String Float Float
    | Group (List (Form msg))
    | Element (Html msg)



-- Creating Forms -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


form : BasicForm msg -> Form msg
form basic =
    Form 0 0 0 1 1 basic []



-- Grouping Forms -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| Takes a list of `Form`s and combines them into a single
`Form`.
-}
group : List (Form msg) -> Form msg
group forms =
    form <| Group forms



-- Manipulating Forms -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -


{-| Move a form by the given amount (x, y). This is a relative translation so
`(move (5,10) form)` would move `form` five pixels to the right and ten pixels up.
-}
move : ( Float, Float ) -> Form msg -> Form msg
move ( x, y ) form =
    { form | x = form.x + x, y = form.y + y }


{-| Move a shape in the x direction. This is relative so `(moveX 10 form)` moves
`form` 10 pixels to the right.
-}
moveX : Float -> Form msg -> Form msg
moveX x =
    move ( x, 0 )


{-| Move a shape in the y direction. This is relative so `(moveY 10 form)` moves
`form` upwards by 10 pixels.
-}
moveY : Float -> Form msg -> Form msg
moveY y =
    move ( 0, y )


{-| Scale a form by a given factor. Scaling by 2 doubles both dimensions,
and quadruples the area.
-}
scale : Float -> Form msg -> Form msg
scale s form =
    { form | scale = form.scale * s }


{-| Rotate a form by a given angle. Rotate takes standard Elm angles (radians)
and turns things counterclockwise. So to turn `form` 30&deg; to the left
you would say, `(rotate (degrees 30) form)`.
-}
rotate : Float -> Form msg -> Form msg
rotate t form =
    { form | theta = form.theta + t }


{-| Set the alpha of a `Form msg`. The default is 1, and 0 is totally transparent.
-}
alpha : Float -> Form msg -> Form msg
alpha a form =
    { form | alpha = a }



-- Shapes ----------------------------------------------------------------------


{-| A polygon or an ellipse. Only describes the size and shape of the figure.
Position, color, thickness, etc. are all specified later.
-}
type Shape
    = Polygon (List Point)
    | Ellipse Float Float


{-| Specifies the styling (color, line, etc.) of a shape.
-}
type alias Style =
    { fill : Texture
    , line : Stroke
    }



-- Creating Shapes -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -


{-| `polygon points` is a polygon bounded by `points`.
-}
polygon : List Point -> Shape
polygon =
    Polygon


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
        Polygon <| List.map f (List.range 0 n)


{-| A rectangle. The arguments specify thickness and height, respectively.
-}
rect : Float -> Float -> Shape
rect w h =
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
    rect n n



-- TODO: add roundedRect and roundedSquare
-- TODO: add triangle


{-| An ellipse. The arugments specify the horizontal and vertical radii,
respectively.
NOTE: called `oval` in original lib
-}
ellipse : Float -> Float -> Shape
ellipse =
    Ellipse


{-| A circle. The argument specifies the radius.
-}
circle : Float -> Shape
circle r =
    ellipse r r



-- Turning Shapes into Forms -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| Fills in a shape, making it into a 'Form'. The argument
specifies the texture of the fill. The line is left transparent.
-}
filled : Texture -> Shape -> Form msg
filled texture =
    styled { fill = texture, line = solid 1 (uniform Color.black) }


{-| Adds a line to a shape, making it into a 'Form'. The arguments
specify the thickness and texture of the line, respectiverly. The fill is
left transparent.
-}
stroked : Stroke -> Shape -> Form msg
stroked stroke =
    styled { fill = transparent, line = stroke }


{-| Adds a fill and line to a 'Shape', making it into a 'Form'. The
first argument specifies the fill texture, and the second two arguments
specify the line thickness and texture, respectively.
-}
filledAndStroked : Texture -> Stroke -> Shape -> Form msg
filledAndStroked texture stroke =
    styled { fill = texture, line = stroke }


{-| Takes a `Shape` and *any* `Style` and converts them into
a `Form`, giving you total control over the styling of the shape.
-}
styled : Style -> Shape -> Form msg
styled style shape =
    form <| Shape shape style



-- Paths -----------------------------------------------------------------------


{-| A segment of a line or curve. Only describes the shape of the line.
Position, color, thickness, etc. are all specified later.
-}
type Path
    = Polyline (List Point)



-- Creating Paths -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| `polyline points` is a polyline with vertices
at `points`. (A polyline is a collection of connected
line segments. It can be thought of as drawing a
"connect-the-dots" line through a list of points.)
-}
path : List Point -> Path
path =
    Polyline


{-| `segment (x1,y1) (x2,y2)` is a line segment with
endpoints at `(x1,y1)` and `(x2,y2)`.
FIXME: rename to `line`?
-}
segment : Point -> Point -> Path
segment a b =
    path [ a, b ]



-- Turning Paths into Forms -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
{-
   -- ORIG
   segment (0,0) (1,1)
       |> traced (dashed red)
   rect 4 5
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
   rect 4 5
       |> filled (uniform red)
       AND
       |> stroked (solid 1) (uniform red)
       OR
       |> stroked (solid 1 (uniform red))

-}


{-| Trace a path with a given line style.
-}
traced : Stroke -> Path -> Form msg
traced stroke path =
    form <| Path path stroke



-- Images ----------------------------------------------------------------------


{-| An image. The arguments specify the image's thickness, height and url.
-}
image : Float -> Float -> String -> Form msg
image w h url =
    form <| Image url w h



-- Raw Content -----------------------------------------------------------------


{-| Creates a `Form` from an arbitrary `Html` element. The
resulting form is subject to all of the regular manipulations.
Note that if you are compiling to SVG, then this functionality
is not supported in Internet Explorer.
-}
html : Html msg -> Form msg
html elem =
    form <| Element elem



-- Styling ---------------------------------------------------------------------
-- Textures -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| Describes the texture of a shape or line. It can be a uniform color,
gradient, or tiled texture.
-}
type Texture
    = Transparent
    | Uniform Color



-- | Gradient Gradient
-- | Pattern Float Float String Float
-- | Texture String


{-| Uniform color fill
-}
uniform : Color -> Texture
uniform =
    Uniform


{-| Transparent texture
-}
transparent : Texture
transparent =
    Transparent



-- Strokes -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


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
type alias Stroke =
    { texture : Texture
    , thickness : Float
    , cap : LineCap
    , join : LineJoin
    , dashing : List ( Int, Int )
    , dashOffset : Int
    }


{-| Creates a Form representing a solid line from a
'Path' object. The first argument specifies the line
thickness and the second argument specifies the texture
to use for the line stroke.
-}
solid : Float -> Texture -> Stroke
solid =
    broken []


{-| The same as `solid`, except the line is dots.
-}
dot : Float -> Texture -> Stroke
dot thickness =
    let
        d =
            round thickness
    in
        broken [ ( d, d ) ] thickness


{-| The same as `solid`, except the line is dashed.
-}
dash : Float -> Texture -> Stroke
dash thickness =
    let
        d =
            round thickness
    in
        broken [ ( d * 5, d * 2 ) ] thickness


{-| Define a dashed line type with the given thickness, where the dashes are longer than normal.
-}
longdash : Float -> Texture -> Stroke
longdash thickness =
    let
        d =
            round thickness
    in
        broken [ ( d * 12, d * 6 ) ] thickness


{-| Define a line type with the given thickness, including alternating dots and dashes.
-}
dotdash : Float -> Texture -> Stroke
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
broken : List ( Int, Int ) -> Float -> Texture -> Stroke
broken dash thickness texture =
    Stroke texture thickness Flat Sharp dash 0


{-| Describes the cap style of a line. `Flat` capped lines have
no endings, `Padded` capped lines have flat endings that extend
slightly past the end of the line, and `Round` capped lines have
hemispherical endings.
-}
type LineCap
    = Flat
    | Round
    | Padded


{-| Describes the join style of a line.
-}
type LineJoin
    = Smooth
    | Sharp
    | Clipped
