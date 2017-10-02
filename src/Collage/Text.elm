module Collage.Text
    exposing
        ( Alignment(..)
        , Face(..)
        , Line(..)
        , Shape(..)
        , Style
        , Text(..)
        , Weight(..)
        , align
        , color
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
        , tiny
        , weight
        , width
        )

{-| TODO

@docs Text, fromString, empty

@docs Style

@docs Face, face, color

@docs size, tiny, small, normal, large, huge, enormous

@docs Shape, shape, Weight, weight, Line, line

@docs Alignment, align

@docs width, height

-}

import Color exposing (Color)
import Native.Text


-- Text ------------------------------------------------------------------------


{-| A line or block of text.
-}
type Text
    = Text Style String


{-| Specifies the styling (color, font, weight, etc.) of text
-}
type alias Style =
    { face : Face
    , size : Int
    , color : Color
    , shape : Shape
    , weight : Weight

    --FIXME: should be Set Line
    , line : Maybe Line
    , align : Alignment
    }



-- Creating Text -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| Creates a line of text. The first argument specifies the font
size (in pts). Font defaults to black sans-serif.
-}
fromString : String -> Text
fromString =
    Text <|
        Style Sansserif normal Color.black Upright Normal Nothing Left


{-| -}
empty : Text
empty =
    fromString ""



-- Styling ---------------------------------------------------------------------


styled : Style -> String -> Text
styled =
    Text



-- Face and Color -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -


{-| -}
type Face
    = Roman
    | Sansserif
    | Monospace
      -- | Cursive
      -- | Fantasy
    | Font String


{-| Sets the font face of `Text`.
FIXME: rename?
-}
face : Face -> Text -> Text
face face (Text style str) =
    Text { style | face = face } str


{-| Gives a `Text` element a solid color.
-}
color : Color -> Text -> Text
color clr (Text style str) =
    Text { style | color = clr } str



-- Size -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| -}
size : Int -> Text -> Text
size size (Text style str) =
    Text { style | size = size } str


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


{-| -}
type Shape
    = Upright
      -- | Smallcaps
      -- | Slanted
    | Italic


{-| Italicizes `Text`.
-}
shape : Shape -> Text -> Text
shape shape (Text style str) =
    Text { style | shape = shape } str


{-| -}
type Weight
    = Normal
      -- | Light
      -- | Medium
      -- | Black
    | Bold


{-| Makes `Text` bold.
-}
weight : Weight -> Text -> Text
weight weight (Text style str) =
    Text { style | weight = weight } str



-- Decoration -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| -}
type Line
    = Under
    | Over
    | Through


{-| Underlines `Text`.
-}
line : Line -> Text -> Text
line line (Text style str) =
    Text { style | line = Just line } str



-- Alignment -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -


{-| Describes the alignment (justification) of a text element.
-}
type Alignment
    = Left
    | Right
      -- | Justify
    | Center


{-| -}
align : Alignment -> Text -> Text
align align (Text style str) =
    Text { style | align = align } str



-- Calculations ----------------------------------------------------------------


{-| -}
width : Text -> Float
width (Text style string) =
    Native.Text.width (toCssFontSpec style) string


{-| -}
height : Text -> Float
height (Text style _) =
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

                Normal ->
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
