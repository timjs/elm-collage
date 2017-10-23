module Helpers exposing (foldrLazy)

-- List ------------------------------------------------------------------------


foldrLazy : (e -> (() -> a) -> a) -> a -> List e -> a
foldrLazy f acc list =
    case list of
        [] ->
            acc

        x :: xs ->
            f x (\() -> foldrLazy f acc xs)
