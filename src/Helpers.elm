module Helpers exposing
  ( orLazy
  , segments
  )

{-| -}

import Helpers.List exposing (rotate)


-- Maybe -----------------------------------------------------------------------


{-| Non-strict version of `or`. The second argument will only be evaluated if the first argument is `Nothing`.
-}
orLazy : Maybe a -> (() -> Maybe a) -> Maybe a
orLazy ma fmb =
  case ma of
    Nothing -> fmb ()
    Just _ -> ma


segments : Bool -> List ( Float, Float ) -> List ( ( Float, Float ), ( Float, Float ) )
segments closed ps =
    List.map2 Tuple.pair ps (rotate ps)
        |> (if closed then
                identity

            else
                List.take (List.length ps - 1)
           )
