module Helpers exposing
  ( orLazy
  )

{-| -}


-- Maybe -----------------------------------------------------------------------


{-| Non-strict version of `or`. The second argument will only be evaluated if the first argument is `Nothing`.
-}
orLazy : Maybe a -> (() -> Maybe a) -> Maybe a
orLazy ma fmb =
  case ma of
    Nothing -> fmb ()
    Just _ -> ma
