module Collage.Text
    exposing
        ( Face(..)
        , Line(..)
        , Shape(..)
        , Style
        , Text
          --(.)
        , Weight(..)
        , color
        , defaultStyle
        , empty
        , enormous
        , face
        , fromString
        , height
        , huge
        , large
        , line
        , normal
        , shape
        , size
        , small
        , style
        , tiny
        , weight
        , width
        )

{-| A library for styling and displaying text.

While the String library focuses on representing and manipulating strings of characters,
the Text library focuses on how those strings should look on screen.
It lets you make text bold or italic, set the typeface, set the text size, etc.


# Text

@docs Text


# Creating text

@docs fromString, empty


# Styling text


## Typeface and color

@docs Face, face, color


## Size

@docs size, tiny, small, normal, large, huge, enormous


## Shape and weight

@docs Shape, shape, Weight, weight


## Decorations

@docs Line, line


## Creating styles

@docs Style, style, defaultStyle


# Measuring text

@docs width, height

-}

import Collage.Core as Core
import Color exposing (Color)
import Native.Text


-- Text ------------------------------------------------------------------------


{-| Represents styled text.

It can be rendered with collages.

-}
type alias Text =
    Core.Text Style



-- Creating Text -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| Convert a string into text which can be styled and displayed.

To show the string "Hello World!" on screen in italics, you could say:

    fromString "Hello World!"
        |> shape Italic
        |> alignment Left
        |> Collage.rendered

-}
fromString : String -> Text
fromString =
    Core.Chunk defaultStyle


{-| Text with nothing in it.

    empty =
        fromString ""

-}
empty : Text
empty =
    fromString ""



-- Style Text -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| Specifies the styling (color, typeface, weight, etc.) of text.
-}
type alias Style =
    { face : Face
    , size : Int
    , color : Color
    , shape : Shape
    , weight : Weight

    --FIXME: should be Set Line
    , line : Maybe Line

    --TODO: todo or not todo?
    -- , alignment : Alignment
    }


{-| Give some text a predefined style.

For example, if you design a style called `heading` that is specifically for heading text,
you could apply it to text like this:

    heading =
        { face = Sansserif
        , size = huge
        , color = Color.darkBlue
        , shape = Upright
        , weight = Bold
        , line = Nothing
        , alignment = Center
        }

    fromString "Welcome to Elm Collage!"
        |> style heading

-}
style : Style -> Text -> Text
style style (Core.Chunk _ string) =
    Core.Chunk style string


{-| Plain black text.

It uses the browsers default typeface and text height.
No decorations are used.

-}
defaultStyle : Style
defaultStyle =
    { face = Sansserif
    , size = normal
    , color = Color.black
    , shape = Upright
    , weight = Regular
    , line = Nothing

    -- , alignment = Left
    }



-- Face and Color -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -


{-| Possible typefaces for text.

`Roman`, `Sansserif` and `Monospace` correspond to the default browser fonts of the user.
Use `Font` to specify a concrete typeface.

-}
type Face
    = Roman
    | Sansserif
    | Monospace
      -- | Cursive
      -- | Fantasy
    | Font String


{-| Set the typeface of some text.
-}
face : Face -> Text -> Text
face face (Core.Chunk style str) =
    Core.Chunk { style | face = face } str


{-| Set the color of some text.
-}
color : Color -> Text -> Text
color color (Core.Chunk style str) =
    Core.Chunk { style | color = color } str



-- Size -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| Set the size of some text.
-}
size : Int -> Text -> Text
size size (Core.Chunk style str) =
    Core.Chunk { style | size = size } str


{-| -}
tiny : Int
tiny =
    11


{-| -}
small : Int
small =
    13


{-| -}
normal : Int
normal =
    16


{-| -}
large : Int
large =
    19


{-| -}
huge : Int
huge =
    23


{-| -}
enormous : Int
enormous =
    27



-- Shape and Weight -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| Possible shapes for a piece of text.
-}
type Shape
    = Upright
      -- | SmallCapped
      -- | Slanted
    | Italic


{-| Set the shape of some text.
-}
shape : Shape -> Text -> Text
shape shape (Core.Chunk style str) =
    Core.Chunk { style | shape = shape } str



-- type Weight
--     = ExtraLight
--     | Light
--     | Medium
--     | SemiBold
--     | Bold
--     | Black


{-| Possible weights for a piece of text.
-}
type Weight
    = Regular
    | Bold


{-| Makes `Text` bold.
-}
weight : Weight -> Text -> Text
weight weight (Core.Chunk style str) =
    Core.Chunk { style | weight = weight } str



-- Decoration -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| Styles for lines on text.
-}
type Line
    = Under
    | Over
    | Through


{-| Put lines on text.

This allows you to add an underline, an overline, or a strike out text:

    line Under   (fromString "underline")
    line Over    (fromString "overline")
    line Through (fromString "strike out")

-}
line : Line -> Text -> Text
line line (Core.Chunk style str) =
    Core.Chunk { style | line = Just line } str



{-
   -- Alignment -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -


   {-| Describes the alignment (justification) of a piece of text.
   -}
   type Alignment
       = Left
       | Right
         -- | Justify
       | Center


   {-| Set the alignment of some text.
   -}
   alignment : Alignment -> Text -> Text
   alignment alignment (Core.Chunk style str) =
       Core.Chunk { style | alignment = alignment } str
-}
-- Calculations ----------------------------------------------------------------


{-| Width of the text when displayed on the user screen.

  - **Warning!**
    Use this function sporadically.
    Although it should be quite fast,
    it calls methods of the canvas object (yes really) on the client which can take some time...

-}
width : Text -> Float
width (Core.Chunk style string) =
    Native.Text.width (toCssFontSpec style) string


{-| Height of the text.

This is equal to the text size:

    fromString "Hello World!"
        |> size 16
        |> height
    ==
    16

-}
height : Text -> Float
height (Core.Chunk style _) =
    toFloat style.size


{-| Example:

```css
/* style | variant | weight | stretch | size/line-height | family */
font: italic small-caps bolder condensed 16px/3 cursive;
```

-}
toCssFontSpec : Style -> String
toCssFontSpec style =
    let
        --NOTE: adding font-stretch makes spec not parse...
        spec =
            [ -- font-style
              case style.shape of
                Italic ->
                    "italic"

                Upright ->
                    "normal"
            , -- font-variant
              "normal"
            , -- font-weight
              case style.weight of
                Bold ->
                    "bold"

                Regular ->
                    "normal"
            , -- font-size
              toString style.size ++ "px"
            , -- font-family
              case style.face of
                Roman ->
                    "serif"

                Sansserif ->
                    "sans-serif"

                Monospace ->
                    "monospace"

                Font name ->
                    name
            ]
    in
    String.concat <| List.intersperse " " <| spec
