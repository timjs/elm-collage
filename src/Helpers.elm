module Helpers exposing (foldrLazy, orLazy, uncurry)

-- General ---------------------------------------------------------------------

uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
  f a b


-- Maybe -----------------------------------------------------------------------

{-| Non-strict version of `or`. The second argument will only be
evaluated if the first argument is `Nothing`.
-}
orLazy : Maybe a -> (() -> Maybe a) -> Maybe a
orLazy ma fmb =
  case ma of
    Nothing -> fmb ()
    Just _ -> ma


-- List ------------------------------------------------------------------------

foldrLazy : (e -> (() -> a) -> a) -> a -> List e -> a
foldrLazy f acc list =
  case list of
    [] -> acc
    x :: xs -> f x (\() -> foldrLazy f acc xs)
