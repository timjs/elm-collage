module Example exposing (Model, Msg, example)

import Browser
import Collage exposing (..)
import Collage.Render exposing (svg)
import Collage.Sketchy as Sketchy exposing (sketchy)
import Html exposing (Html)
import Html.Attributes
import Html.Events


type alias Config msg model =
    { init : model
    , update : msg -> model -> model
    , render : model -> Collage msg
    , view : Html msg -> Html msg
    }


type Msg childMsg
    = ClickedNormal
    | ClickedSketchy
    | ChildMsg childMsg


type Renderer
    = Normal
    | Sketchy


type alias Model childMsg childModel =
    { collage : Collage childMsg
    , renderer : Renderer
    , model : childModel
    , sketchyConfig : Sketchy.Config
    }


init : (childModel -> Collage childMsg) -> childModel -> ( Model childMsg childModel, Cmd (Msg childMsg) )
init render child =
    let
        model =
            Model (render child) Normal child Sketchy.defaultConfig
    in
    ( model, Cmd.none )


update : Config childMsg childModel -> Msg childMsg -> Model childMsg childModel -> ( Model childMsg childModel, Cmd (Msg childMsg) )
update config msg model =
    let
        render m =
            case m.renderer of
                Sketchy ->
                    ( { m | collage = sketchy m.sketchyConfig (config.render m.model) }
                    , Cmd.none
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
            { model | renderer = Sketchy, sketchyConfig = Sketchy.nextSeed model.sketchyConfig }
                |> render

        ChildMsg cMsg ->
            { model | model = config.update cMsg model.model }
                |> render


example : Config childMsg childModel -> Platform.Program flags (Model childMsg childModel) (Msg childMsg)
example config =
    Browser.element
        { init = \_ -> init config.render config.init
        , update = update config
        , subscriptions = \_ -> Sub.none
        , view = view config.view
        }


button : String -> Msg childMsg -> Html (Msg childMsg)
button name msg =
    Html.button [ Html.Events.onClick msg ] [ Html.text name ]


view : (Html childMsg -> Html childMsg) -> Model childMsg childModel -> Html (Msg childMsg)
view childView model =
    Html.div []
        [ Html.div [ Html.Attributes.style "margin-bottom" "10px" ]
            [ button "Normal" ClickedNormal
            , button "Sketchy" ClickedSketchy
            ]
        , model.collage
            |> svg
            |> childView
            |> Html.map ChildMsg
        ]
