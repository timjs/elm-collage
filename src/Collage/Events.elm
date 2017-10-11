module Collage.Events
    exposing
        ( on
        , onClick
        , onDoubleClick
        , onFocusIn
        , onFocusOut
        , onMouseDown
        , onMouseEnter
        , onMouseLeave
        , onMouseOut
        , onMouseOver
        , onMouseUp
        )

{-| Use this module to make your graphics interactive.
It is as easy as you think it is.

    collage
        |> onClick Clicked

Will send the message `Clicked` to your update function where you can handle it.
You will probably need some way to identify your objects to keep track of _which_ object the user clicked on:

    drawing.collage
        |> onClick (ClickedOn drawing.id)

where `drawing : { r | collage : Collage, id : Id }`


### Contents

  - [Mouse events](#mouse-events)
  - [Focus events](#focus-events)
  - [Custom events](#custom-events)


# Mouse events

@docs onClick, onDoubleClick, onMouseDown, onMouseUp, onMouseEnter, onMouseLeave, onMouseOver, onMouseOut


# Focus events

@docs onFocusIn, onFocusOut


# Custom events

@docs on

-}

import Collage exposing (Collage, Point)
import Json.Decode as Json exposing (field)


-- Events ----------------------------------------------------------------------


{-| Adds a custom event handler to a collage.

The first argument specifies the event name
(as you would give it to JavaScript's `addEventListener`).
The second argument will be used to decode the Json response from the event listener.
If the decoder succeeds,
the resulting message will be passed along to your `update` function.

    onClick : msg -> Collage msg -> Collage msg
    onClick msg =
        on "click" (Json.succeed msg)

-}
on : String -> Json.Decoder msg -> Collage msg -> Collage msg
on event decoder collage =
    { collage | handlers = ( event, decoder ) :: collage.handlers }


simpleOn : String -> msg -> Collage msg -> Collage msg
simpleOn event =
    on event << Json.succeed


mouseOn : String -> (Point -> msg) -> Collage msg -> Collage msg
mouseOn event msg =
    on event <|
        Json.map msg <|
            Json.map2
                (\x y -> ( x, y ))
                (field "clientX" Json.float)
                (field "clientY" Json.float)


{-| -}
onClick : msg -> Collage msg -> Collage msg
onClick =
    simpleOn "click"


{-| -}
onDoubleClick : msg -> Collage msg -> Collage msg
onDoubleClick =
    simpleOn "dblclick"


{-| -}
onMouseDown : (Point -> msg) -> Collage msg -> Collage msg
onMouseDown =
    mouseOn "mousedown"


{-| -}
onMouseUp : (Point -> msg) -> Collage msg -> Collage msg
onMouseUp =
    mouseOn "mouseup"


{-| -}
onMouseEnter : (Point -> msg) -> Collage msg -> Collage msg
onMouseEnter =
    mouseOn "mouseenter"


{-| -}
onMouseLeave : (Point -> msg) -> Collage msg -> Collage msg
onMouseLeave =
    mouseOn "mouseleave"


{-| -}
onMouseOver : (Point -> msg) -> Collage msg -> Collage msg
onMouseOver =
    mouseOn "mouseover"


{-| -}
onMouseOut : (Point -> msg) -> Collage msg -> Collage msg
onMouseOut =
    mouseOn "mouseout"


{-| -}
onMouseMove : (Point -> msg) -> Collage msg -> Collage msg
onMouseMove =
    mouseOn "mousemove"


{-| -}
onFocusIn : msg -> Collage msg -> Collage msg
onFocusIn =
    simpleOn "focusin"


{-| -}
onFocusOut : msg -> Collage msg -> Collage msg
onFocusOut =
    simpleOn "focusout"
