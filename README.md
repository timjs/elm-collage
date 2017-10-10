# Welcome to Elm Collage!

With this library, you can create interactive scalable vector graphics.
Its interface is based on the classic [Elm Graphics library](http://package.elm-lang.org/packages/evancz/elm-graphics/latest).
However, you'll find that a couple of things are different.
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
It has good performance, and excellent hit detection.

You can find a couple of examples in the `examples` directory of the repository.


## Have fun!

This library is under construction.
Please fill out issues on GitHub and help making Elm Collage awesome!
