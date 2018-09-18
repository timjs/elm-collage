module Helpers exposing (foldrLazy, orLazy, uncurry, values)

-- General ---------------------------------------------------------------------


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
  f a b



-- Maybe -----------------------------------------------------------------------


{-| Non-strict version of `or`. The second argument will only be evaluated if the first argument is `Nothing`.
-}
orLazy : Maybe a -> (() -> Maybe a) -> Maybe a
orLazy ma fmb =
  case ma of
    Nothing -> fmb ()
    Just _ -> ma


{-| Convert a list of `Maybe a` to a list of `a` only for the values different from `Nothing`.

    values [ Just 1, Nothing, Just 2 ] == [ 1, 2 ]

-}
values : List (Maybe a) -> List a
values = List.foldr foldrValues []


foldrValues : Maybe a -> List a -> List a
foldrValues item list =
  case item of
    Nothing -> list
    Just v -> v :: list



-- List ------------------------------------------------------------------------


foldrLazy : (e -> (() -> a) -> a) -> a -> List e -> a
foldrLazy f acc list =
  case list of
    [] -> acc
    x :: xs -> f x (\() -> foldrLazy f acc xs)
