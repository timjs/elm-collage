module Collage.Events
    exposing
        ( onClick
        , onMouseDown
        , onMouseUp
        , onMouseOver
        , onMouseOut
        , onFocusIn
        , onFocusOut
        , on
        )

{-|


# Events


## Mouse Events

@docs onClick, onMouseDown, onMouseUp, onMouseOver, onMouseOut


## Focus Events

@docs onFocusIn, onFocusOut


## Custom Events

@docs on

-}

import Json.Decode as Json exposing (field)
import Collage exposing (Point, Form)


-- Events ----------------------------------------------------------------------


{-| Adds a custom event handler to a `Form`. The first
argument specifies the event name (as you would give it
to JavaScript's `addEventListener`). The second argument
will be used to decode the JSON response from the event
listener. If the decoder succeeds, the resulting message
will be passed along to your `update` function.

    onClick : msg -> Form msg -> Form msg
    onClick msg =
        on "click" (Json.succeed msg)

-}
on : String -> Json.Decoder msg -> Form msg -> Form msg
on event decoder f =
    { f | handlers = ( event, decoder ) :: f.handlers }


simpleOn : String -> msg -> Form msg -> Form msg
simpleOn event =
    on event << Json.succeed


mouseOn : String -> (Point -> msg) -> Form msg -> Form msg
mouseOn event msg =
    on event <|
        Json.map msg <|
            Json.map2
                (\x y -> ( x, y ))
                (field "clientX" Json.float)
                (field "clientY" Json.float)


{-| -}
onClick : msg -> Form msg -> Form msg
onClick =
    simpleOn "click"


{-| -}
onMouseDown : (Point -> msg) -> Form msg -> Form msg
onMouseDown =
    mouseOn "mousedown"


{-| -}
onMouseUp : (Point -> msg) -> Form msg -> Form msg
onMouseUp =
    mouseOn "mouseup"


{-| -}
onMouseMove : (Point -> msg) -> Form msg -> Form msg
onMouseMove =
    mouseOn "mousemove"


{-| -}
onMouseOver : (Point -> msg) -> Form msg -> Form msg
onMouseOver =
    mouseOn "mouseover"


{-| -}
onMouseOut : (Point -> msg) -> Form msg -> Form msg
onMouseOut =
    mouseOn "mouseout"


{-| -}
onFocusIn : msg -> Form msg -> Form msg
onFocusIn =
    simpleOn "focusin"


{-| -}
onFocusOut : msg -> Form msg -> Form msg
onFocusOut =
    simpleOn "focusout"
