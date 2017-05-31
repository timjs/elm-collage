module Text
    exposing
        ( Text(..)
        , fromString
        , empty
        , Style
        , Family(..)
        , family
        , monospace
        , size
        , color
        , Shape(..)
        , italic
        , Weight(..)
        , bold
        , Line(..)
        , line
        , Alignment(..)
        , align
        )

{-| TODO

@docs Text, fromString, empty

@docs Style

@docs Family, family, monospace, size, color

@docs Shape, italic, Weight, bold, Line, line

@docs Alignment, align

-}

import Color exposing (Color)


-- Text ------------------------------------------------------------------------


{-| A line or block of text.
-}
type Text
    = Text Style String



-- Creating Text -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| Creates a line of text. The first argument specifies the font
size (in pts). Font defaults to black sans-serif.
-}
fromString : String -> Text
fromString =
    Text <|
        Style Sansserif Nothing Color.black Upright Normal Nothing Left


{-| -}
empty : Text
empty =
    fromString ""



-- Styling ---------------------------------------------------------------------


{-| Specifies the styling (color, font, weight, etc.) of text
-}
type alias Style =
    { family : Family
    , size : Maybe Int
    , color : Color
    , shape : Shape
    , weight : Weight

    --FIXME: should be Set Line
    , line : Maybe Line
    , align : Alignment
    }

styled : Style -> String -> Text
styled =
    Text



-- Family and Size -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -


{-| -}
type Family
    = Roman
    | Sansserif
    | Monospace
      -- | Cursive
      -- | Fantasy
    | Font String


{-| Sets the font family of `Text`.
-}
family : Family -> Text -> Text
family family (Text style str) =
    Text { style | family = family } str

{-|-}
monospace : Text -> Text
monospace = family Monospace

{-| -}
size : Int -> Text -> Text
size size (Text style str) =
    Text { style | size = Just size } str



-- Color -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| Gives a `Text` element a solid color.
-}
color : Color -> Text -> Text
color clr (Text style str) =
    Text { style | color = clr } str



-- Shape and Weight -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{-| -}
type Shape
    = Upright
      -- | Smallcaps
      -- | Slanted
    | Italic


{-| Italicizes `Text`.
-}
italic : Text -> Text
italic (Text style str) =
    Text { style | shape = Italic } str


{-| -}
type Weight
    = Normal
      -- | Light
      -- | Medium
      -- | Black
    | Bold


{-| Makes `Text` bold.
-}
bold : Text -> Text
bold (Text style str) =
    Text { style | weight = Bold } str



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
