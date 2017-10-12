module Collage.Text
    exposing
        ( Line(..)
        , Shape(..)
        , Style
        , Text
          --(.)
        , Typeface(..)
        , Weight(..)
        , color
        , defaultStyle
        , empty
        , enormous
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
        , typeface
        , weight
        , width
        )

{-| A library for styling and displaying text.

While the String library focuses on representing and manipulating strings of characters,
the Text library focuses on how those strings should look on screen.
It lets you make text bold or italic, set the typeface, set the text size, etc.

Be aware that this module is intended for **small chunks of text** on **one line**.
Using a newline character in a text chunk will simply have no effect.
Alignment and placement should be done using the Collage or Layout modules.

To add some text to your collages,
follow the next steps:

1.  Create some text chunk with `fromString`.
2.  Style it using the functions in this module.
3.  Make a collage out of it, i.e. render it using `Collage.rendered`.
    From this point, you cannot style your text any more.
    What you _can_ do however is...
4.  Transform the collage using the functions in the Collage module:
    shift it, rotate it, scale it etc.
    Or use the placement functions in Collage.Layout.

So the most important thing to remember is that after you have turned your text into a collage,
you cannot style it anymore!

**Note:**
Function and type names in this module clash with those from Collage and Collage.Layout.
Best way to use is a qualified import like:

    import Collage.Text as Text exposing (Text, fromString)


### Contents

  - [Basics](#basics)
  - [Creating text](#creating-text)
  - [Styling text](#styling-text)
      - [Typeface and color](#typeface-and-color)
      - [Size](#size)
      - [Shape and weight](#shape-and-weight)
      - [Decorations](#decorations)
      - [Creating styles](#creating-styles)
  - [Measuring text](#measuring-text)


# Basics

@docs Text


# Creating text

@docs fromString, empty


# Styling text


## Typeface and color

@docs Typeface, typeface, color


## Size

The predefined font sizes below are spaced `sqrt (sqrt 2)` of each other.
which gives a balanced view.
This idea is bluntly stolen from LaTeX,
which does something quite similar.
Off course, you can always specify your own font size explicitly.

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


{-| Opaque type representing styled text.
-}
type alias Text =
    Core.Text Style



-- Creating Text -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| Convert a string into text which can be styled and displayed.

To show the string "Hello World!" on screen in large, dark red, italics, you could say:

    fromString "Hello World!"
        |> size large
        |> color Color.darkRed
        |> shape Italic
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
    { typeface : Typeface
    , size : Int
    , color : Color
    , shape : Shape
    , weight : Weight
    , line : Line
    }


{-| Give some text a predefined style.

For example, if you design a style called `heading` that is specifically for heading text,
you could apply it to text like this:

    heading =
        { typeface = Sansserif
        , size = huge
        , color = Color.darkBlue
        , shape = Upright
        , weight = Bold
        , line = Nothing
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

    defaultStyle =
        { typeface = Sansserif
        , size = normal
        , color = Color.black
        , shape = Upright
        , weight = Regular
        , line = None
        }

-}
defaultStyle : Style
defaultStyle =
    { typeface = Sansserif
    , size = normal
    , color = Color.black
    , shape = Upright
    , weight = Regular
    , line = None
    }



-- Typeface and Color -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -


{-| Possible typefaces for text.

`Serif`, `Sansserif`, and `Monospace` correspond to the default browser fonts of the user.
Use `Font` to specify a concrete typeface.

-}
type Typeface
    = Serif
    | Sansserif
    | Monospace
    | Font String


{-| Set the typeface of some text.

    fromString "Text in my favorite font"
        |> typeface (Font "Lato")

-}
typeface : Typeface -> Text -> Text
typeface typeface (Core.Chunk style str) =
    Core.Chunk { style | typeface = typeface } str


{-| Set the color of some text.

Use the Color module to specify colors.

    fromString "Nice blue text"
        |> color Color.blue

-}
color : Color -> Text -> Text
color color (Core.Chunk style str) =
    Core.Chunk { style | color = color } str



-- Size -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| Set the size of some text.

    fromString "Big text"
        |> size huge

-}
size : Int -> Text -> Text
size size (Core.Chunk style str) =
    Core.Chunk { style | size = size } str


{-| 11 px
-}
tiny : Int
tiny =
    11


{-| 13 px
-}
small : Int
small =
    13


{-| 16 px
-}
normal : Int
normal =
    16


{-| 19 px
-}
large : Int
large =
    19


{-| 23 px
-}
huge : Int
huge =
    23


{-| 27 px
-}
enormous : Int
enormous =
    27



-- Shape, Weight and Stretch -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| Possible shapes for a piece of text.
-}
type Shape
    = Upright
    | SmallCaps
    | Slanted
    | Italic


{-| Set the shape of some text.

    fromString "Italic text"
        |> shape Italic

-}
shape : Shape -> Text -> Text
shape shape (Core.Chunk style str) =
    Core.Chunk { style | shape = shape } str


{-| Possible weights for a piece of text.
-}
type Weight
    = Thin
    | Light
    | Regular
    | Medium
    | SemiBold
    | Bold
    | Black


{-| Set the weight of some text.

    fromString "Bold text"
        |> weight Bold

-}
weight : Weight -> Text -> Text
weight weight (Core.Chunk style str) =
    Core.Chunk { style | weight = weight } str


type Stretch
    = Condensed
    | Normal
    | Expanded



{-
   stretch : Stretch -> Text -> Text
   stretch stretch (Core.Chunk style str) =
       Core.Chunk { style | stretch = stretch } str
-}
-- Decoration -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| Styles for lines on or over some text.
-}
type Line
    = None
    | Under
    | Over
    | Through


{-| Put lines on text.

This allows you to add an underline, an overline, or strike out text:

    line None    (fromString "normal text")
    line Under   (fromString "underline")
    line Over    (fromString "overline")
    line Through (fromString "strike out")

-}
line : Line -> Text -> Text
line line (Core.Chunk style str) =
    Core.Chunk { style | line = line } str



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


{-| The width of the text when displayed on the user screen.

  - **Warning!**
    Use this function sporadically.
    Although it should be quite fast,
    it calls methods of the canvas object (yes really) on the client which can take some time...

-}
width : Text -> Float
width ((Core.Chunk style string) as text) =
    -- height text / 2 * toFloat (String.length string)
    Native.Text.width (toCssFontSpec style) string


{-| The height of the text when displayed on the user screen.

This is equal to the text size:

    fromString "Hello World!"
        |> size 16
        |> height    ==    16

(Now you know why newlines are a bad idea...)

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
                Upright ->
                    "normal"

                SmallCaps ->
                    "normal"

                Slanted ->
                    "oblique"

                Italic ->
                    "italic"
            , -- font-variant
              case style.shape of
                SmallCaps ->
                    "small-caps"

                _ ->
                    "normal"
            , -- font-weight
              case style.weight of
                Thin ->
                    "200"

                Light ->
                    "300"

                Regular ->
                    "normal"

                Medium ->
                    "500"

                SemiBold ->
                    "600"

                Bold ->
                    "bold"

                Black ->
                    "800"
            , -- font-size
              toString style.size ++ "px"
            , -- font-family
              case style.typeface of
                Serif ->
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
