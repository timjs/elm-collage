# Welcome to Elm Collage!

With this library you can create interactive, modular, scalable vector graphics.
Its interface is loosly based on the classic [Elm Graphics library](http://package.elm-lang.org/packages/evancz/elm-graphics/latest).
However, you'll find that a couple of things are different.
You can find all the details in the module documentation.

This library consists of four main modules:

1. `Collage`
2. `Collage.Events`
3. `Collage.Text`
4. `Collage.Layout`

In `Collage` you can find functions to create and style _shapes_ and _paths_.
After turning them into a _collage_ you can shift, scale and rotate them,
group them togehter with other collages, shift, scale and rotatem them again,
and so on, and so on.

Using `Collage.Events` every part of you collage can be made interactive by sending messages,
like you're used to when using `Html` or pure `Svg`.

The `Collage.Text` module contains functions to make up text,
which can be rendered inside your collage.
It can calculate the width and height your text will occupy on the client's screen.

With `Collage.Layout` you can

## Give me an example!

```elm
```

## How does it compare to library X?
### Interactive graphics

You can make your collages interactive by using the events from the Collage.Event module.
See the documentation of that module for more information.


### Automatic relative positioning

The Collage.Layout module is designed to help you place collages with respect to each other.
By keeping track of the dimensions,
the module can place collages next to each other, above each other,
align them to the left, to the top, etc.


## Main Design Goals

  - You can create _shapes_ like rectangles, circles, ellipses, polygons etc.
    By filling and/or outlining them, you turn them into a _collage_.

  - Something similar applies to _paths_.
    You can draw lines, line segments and paths.
    (Bezier curves are on the todo list!)
    By _tracing_ them, you turn them into a _collage_.

  - Other ways to create a collage are by including text, images or raw Html.

  - A _collage_ itself can than be transformed and grouped.
    Transformations include shifting, scaling, rotating, skewing, mirroring etc.
    By grouping collages they can be transformed as one.

  - Events can easily be added to a collage using the `Collage.Event` module.

  - You can laying out multiple collages in a compositional way by using the `Collage.Layout` module.
    This is similar to the functionality provided by the old `Graphics.Element` module
    and promoted by the popular Haskell [Diagrams library]().
