module Collage.Sketchy.Helpers exposing (rotateList, segments)

import Collage exposing (Point)


segments : Bool -> List Point -> List ( Point, Point )
segments closed ps =
    List.map2 Tuple.pair ps (rotateList ps)
        |> (if closed then
                identity

            else
                List.take (List.length ps - 1)
           )


rotateList : List a -> List a
rotateList list =
    case list of
        head :: tail ->
            tail ++ [ head ]

        _ ->
            list
