module Example exposing (Model, Msg, example)

import Browser
import Collage exposing (..)
import Collage.Render exposing (svg)
import Collage.Sketchy as Sketchy exposing (sketchy)
import Html exposing (Html)
import Html.Events
import Random


type alias Config msg model =
    { init : model
    , update : msg -> model -> model
    , render : model -> Collage msg
    }


type Msg childMsg
    = ClickedNormal
    | ClickedSketchy
    | GeneratedSketchy (Collage childMsg)
    | ChildMsg childMsg


type Renderer
    = Normal
    | Sketchy


type alias Model childMsg childModel =
    { collage : Collage childMsg
    , renderer : Renderer
    , model : childModel
    }


init : (childModel -> Collage childMsg) -> childModel -> ( Model childMsg childModel, Cmd (Msg childMsg) )
init render child =
    let
        model =
            Model (render child) Normal child
    in
    ( model, Cmd.none )


update : Config childMsg childModel -> Msg childMsg -> Model childMsg childModel -> ( Model childMsg childModel, Cmd (Msg childMsg) )
update config msg model =
    let
        debug =
            Debug.log "msg" msg

        render m =
            case m.renderer of
                Sketchy ->
                    ( m
                    , sketchy Sketchy.defaultConfig (config.render m.model)
                        |> Random.generate GeneratedSketchy
                    )

                Normal ->
                    ( { m | collage = config.render m.model }
                    , Cmd.none
                    )
    in
    case msg of
        ClickedNormal ->
            { model | renderer = Normal }
                |> render

        ClickedSketchy ->
            { model | renderer = Sketchy }
                |> render

        GeneratedSketchy collage ->
            ( { model | collage = collage }, Cmd.none )

        ChildMsg cMsg ->
            { model | model = config.update cMsg model.model }
                |> render


example : Config childMsg childModel -> Platform.Program flags (Model childMsg childModel) (Msg childMsg)
example config =
    Browser.element
        { init = \_ -> init config.render config.init
        , update = update config
        , subscriptions = \_ -> Sub.none
        , view = view
        }


button : String -> Msg childMsg -> Html (Msg childMsg)
button name msg =
    Html.button [ Html.Events.onClick msg ] [ Html.text name ]


view : Model childMsg childModel -> Html (Msg childMsg)
view model =
    Html.div []
        [ Html.div []
            [ button "Normal" ClickedNormal
            , button "Sketchy" ClickedSketchy
            ]
        , model.collage
            |> svg
            |> Html.map ChildMsg
        ]
