module Text
    exposing
        ( Text(..)
        , fromString
        , empty
        , Style
        , Face(..)
        , face
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

@docs Face, face, monospace, size, color

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
    { face : Face
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



-- Face and Size -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -


{-| -}
type Face
    = Roman
    | Sansserif
    | Monospace
      -- | Cursive
      -- | Fantasy
    | Font String


{-| Sets the font face of `Text`.
-}
face : Face -> Text -> Text
face face (Text style str) =
    Text { style | face = face } str

{-|-}
monospace : Text -> Text
monospace = face Monospace

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
