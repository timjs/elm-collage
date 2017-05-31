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
import Collage exposing (Point, Collage)


-- Events ----------------------------------------------------------------------


{-| Adds a custom event handler to a `Collage`. The first
argument specifies the event name (as you would give it
to JavaScript's `addEventListener`). The second argument
will be used to decode the JSON response from the event
listener. If the decoder succeeds, the resulting message
will be passed along to your `update` function.

    onClick : msg -> Collage msg -> Collage msg
    onClick msg =
        on "click" (Json.succeed msg)

-}
on : String -> Json.Decoder msg -> Collage msg -> Collage msg
on event decoder f =
    { f | handlers = ( event, decoder ) :: f.handlers }


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
onMouseDown : (Point -> msg) -> Collage msg -> Collage msg
onMouseDown =
    mouseOn "mousedown"


{-| -}
onMouseUp : (Point -> msg) -> Collage msg -> Collage msg
onMouseUp =
    mouseOn "mouseup"


{-| -}
onMouseMove : (Point -> msg) -> Collage msg -> Collage msg
onMouseMove =
    mouseOn "mousemove"


{-| -}
onMouseOver : (Point -> msg) -> Collage msg -> Collage msg
onMouseOver =
    mouseOn "mouseover"


{-| -}
onMouseOut : (Point -> msg) -> Collage msg -> Collage msg
onMouseOut =
    mouseOn "mouseout"


{-| -}
onFocusIn : msg -> Collage msg -> Collage msg
onFocusIn =
    simpleOn "focusin"


{-| -}
onFocusOut : msg -> Collage msg -> Collage msg
onFocusOut =
    simpleOn "focusout"
