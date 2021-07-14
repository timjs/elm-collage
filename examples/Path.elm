module Path exposing (main)

import Example
import Collage exposing (..)
import Collage.Layout exposing (..)
import Color exposing (..)
import Html exposing (Html)


zigzag : Collage msg
zigzag =
    let
        points =
            [ ( 0, 50 )
            , ( 50, 0 )
            , ( 50, 50 )
            , ( 100, 0 )
            , ( 100, 50 )
            , ( 150, 0 )
            ]
    in
    vertical
        [ path points
                |> traced (solid thin (uniform red))
        , spacer 0 20
        , curve points
                |> traced (solid thin (uniform red))
        , spacer 0 20
        ]


main : Platform.Program () (Example.Model () (Collage ())) (Example.Msg ())
main =
    Example.example
        { init = zigzag
        , update = (\_ _ -> zigzag)
        , render = (\_ -> zigzag)
        , view = identity
        }
