module Collage.Sketchy.Helpers exposing (rotate, segments)

import Collage exposing (Point)


segments : Bool -> List Point -> List ( Point, Point )
segments closed ps =
    List.map2 Tuple.pair ps (rotate ps)
        |> (if closed then
                identity

            else
                List.take (List.length ps - 1)
           )


rotate : List a -> List a
rotate list =
    case list of
        head :: tail ->
            tail ++ [ head ]

        _ ->
            list
