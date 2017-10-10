# Welcome to Elm Collage!

With this library, you can create interactive scalable vector graphics.
Its interface is based on the classic [Elm Graphics library](http://package.elm-lang.org/packages/evancz/elm-graphics/latest).
However, you'll find that a couple of things are different
and a lot of functionality has been added.
You can find all the details in the module documentation.

The library consists of four main modules:

1. `Collage`
2. `Collage.Events`
3. `Collage.Text`
4. `Collage.Layout`

In `Collage` you can find functions to create and style _shapes_ and _paths_.
After turning them into a _collage_ you can shift, scale and rotate them,
group them together with other collages, shift, scale and rotate them again,
and so on, and so on.
Including images or raw Html also belongs to the possibilities.

Using `Collage.Events` every part of your collage can be made interactive by sending messages,
like you're used to when using `Html` or pure `Svg`.

The `Collage.Text` module contains functions to make up chunks of text,
which can be rendered inside your collage.
And of course, you can shift, scale, and rotate text as you please,
after you turned them into a collage.
The module can calculate the width and height your text will occupy on the client's screen.

With `Collage.Layout` you can position collages relative to each other.
By keeping track of the dimensions of each collage,
this module can place them next to each other, above each other, align them to the left, to the top, etc.
It is based on the excellent [Diagrams library](https://archives.haskell.org/projects.haskell.org/diagrams/)
by Brent Yorgey.
This is similar to the functionality provided by the old `Graphics.Element` module,
but more powerful.

Although theoretically, there could be multiple backends to render collages,
for now, we only provide a Svg backend in `Collage.Render`.
It has good performance and excellent hit detection.


## Please give me an example!

Ok, here is an example of a blue rectangle and on its top left corner a red circle:

```elm
import Collage exposing (circle, rectangle, filled, uniform)
import Collage.Layout exposing (at, topLeft)
import Collage.Render exposing (svg)
import Color

main =
    let
        circ =
            circle 50
                |> filled (uniform Color.red)

        rect =
            rectangle 200 100
                |> filled (uniform Color.blue)
    in
    rect
        |> at topLeft circ
        |> svg
```

You can find more examples in the [examples directory](https://github.com/timjs/elm-collage/tree/master/examples)
of the repository.


## How does this library compare to...?

  - [Elm Graphics](http://package.elm-lang.org/packages/evancz/elm-graphics/1.0.1)

    This was the original Elm drawing library.
    It is a big inspiration and you'll find a lot of similarities,
    from the way you draw shapes and paths, to styling them.

    In Elm Collage we do not make the distinction between Forms and Elements,
    there are just Collages.
    After Elm's [transition to commands and subscriptions](http://elm-lang.org/blog/farewell-to-frp),
    Elements could not be interactive anymore.
    Collages can be made interactive in the same way you are used to with the Elm Html and Svg libraries.

  - [Elm Render](http://package.elm-lang.org/packages/Kwarrtz/render/2.0.0)

    Elm Collage is actually a fork of this excellent work by @Kwarrtz.
    The module organisation changed a bit,
    as well as some styling functions,
    but the code to render Svg is almost untouched.
    In this reincarnation, you will find new ways to position and align your graphics
    using the Collage.Layout module.

  - [Elm GraphicSVG](http://package.elm-lang.org/packages/MacCASOutreach/graphicsvg/2.1.0)

    Also an inspiration for this library.
    However, I think Elm Collage is a bit more structured
    and provides more guidance by using well-chosen types.
    You cannot make a rectangle "italic" in Elm Collage for example.

  - [Elm Diagrams](http://package.elm-lang.org/packages/vilterp/elm-diagrams/7.2.0)

    @vilterp's Elm port of the great Haskell library with the same name.
    You can create all sorts of diagrams and position them next to, above, and atop of each other.
    It used to render using Elm Graphics
    and implement its own hit detection and mouse interaction.

    Elm Diagrams is not ported to Elm 0.18.
    In Elm Collage we use the browsers native hit detection when rendering to Svg.
    Also, we explicitly separate creating and styling forms on one hand,
    and transforming collages on the other.
    Although the layout capabilities of Elm Collage are not as extensive as those in Elm Diagrams,
    I think the Api is a bit simpler.

  - [Haskell Diagrams](https://archives.haskell.org/projects.haskell.org/diagrams/)

    Our big cousin!
    It is awesome and overly complex.
    Interaction is [hard](http://www.cmears.id.au/articles/diagrams-gtk-mouse.html) though...

  - [Clean Scalable Graphics](https://dl.acm.org/citation.cfm?id=2746329)

    Our small cousin!
    First to introduce interaction.
    Does not have such nice layout combinators as other libraries though...


## Have fun!

This library is under construction.
Please fill out issues on GitHub and help to make Elm Collage awesome!
