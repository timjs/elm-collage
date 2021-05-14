module Collage.Sketchy exposing (sketchy)

import Collage exposing (Collage, Point)
import Collage.Core as Core
import Random
import Random.Extra


sketchy : Collage msg -> Random.Generator (Collage msg)
sketchy collage =
    case collage.basic of
        Core.Path style path ->
            case path of
                Core.Polyline ps ->
                    Random.list (List.length ps) (Random.pair (Random.float 0 10) (Random.float 0 10))
                        |> Random.map
                            (\shifts ->
                                List.map2
                                    (\( x, y ) ( shiftX, shiftY ) ->
                                        ( x + shiftX, y + shiftY )
                                    )
                                    ps
                                    shifts
                            )
                        |> Random.map
                            (\points ->
                                { collage | basic = Core.Path style (Core.Polyline points) }
                            )

        Core.Shape ( fill, line ) path ->
            case path of
                Core.Polygon ps ->
                    Random.list (List.length ps) (Random.pair (Random.float 0 10) (Random.float 0 10))
                        |> Random.map
                            (\shifts ->
                                List.map2
                                    (\( x, y ) ( shiftX, shiftY ) ->
                                        ( x + shiftX, y + shiftY )
                                    )
                                    ps
                                    shifts
                            )
                        |> Random.map
                            (\points ->
                                { collage | basic = Core.Shape ( fill, line ) (Core.Polygon points) }
                            )

                Core.Rectangle w h r ->
                    let
                        ps =
                            [ ( -w / 2, -h / 2 )
                            , ( w / 2, -h / 2 )
                            , ( w / 2, h / 2 )
                            , ( -w / 2, h / 2 )
                            ]
                    in
                    Random.list 4 (Random.pair (Random.float 0 10) (Random.float 0 10))
                        |> Random.map
                            (\shifts ->
                                List.map2
                                    (\( x, y ) ( shiftX, shiftY ) ->
                                        ( x + shiftX, y + shiftY )
                                    )
                                    ps
                                    shifts
                            )
                        |> Random.map
                            (\points ->
                                { collage | basic = Core.Shape ( fill, line ) (Core.Polygon points) }
                            )

                _ ->
                    Random.constant collage

        Core.Group collages ->
            Random.Extra.combine (List.map sketchy collages)
                |> Random.map (\group -> { collage | basic = Core.Group group })

        Core.Subcollage fore back ->
            Random.Extra.combine (List.map sketchy [ fore, back ])
                |> Random.map (\group -> { collage | basic = Core.Group group })

        _ ->
            Random.constant collage
