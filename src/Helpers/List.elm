module Helpers.List exposing
    ( values
    , foldrLazy
    , rotate
    , segments
    )


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


foldrLazy : (e -> (() -> a) -> a) -> a -> List e -> a
foldrLazy f acc list =
  case list of
    [] -> acc
    x :: xs -> f x (\() -> foldrLazy f acc xs)


rotate : List a -> List a
rotate list =
    case list of
        head :: tail ->
            tail ++ [ head ]

        _ ->
            list


segments : Bool -> List a -> List ( a, a )
segments closed ps =
    List.map2 Tuple.pair ps (rotate ps)
        |> (if closed then
                identity

            else
                List.take (List.length ps - 1)
           )
