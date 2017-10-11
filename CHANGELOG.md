# Changelog


## [1.2.0](https://github.com/timjs/elm-collage/compare/1.1.0...1.2.0) (2017-10-11)

### Added

  - Collage.Layout
    - A `facing` function to calculate the facing direction.
      Will replace `opposite` in the next major version to avoid name clash with `Collage.opposite`
  - Flowchart example
    - Shows modular creation of collages
    - Uses width and height calculations of subelements
  - Table of contents to each exported module

### Depricated

  - Collage.Layout
    - `opposite` (in favor of `facing`)

### Changed

  - Correct layout rules for line thickness of shapes
    - Keep track of line thickness and correct for it when calculating the envelopes of shapes
    - Idem for paths, but only if the line cap is _not_ flat

### Fixed

  - Rotation of triangle in axis example
  - Description of arguments of dotted styles
  - Rendering of origin and envelope for debugging


## [1.1.0](https://github.com/timjs/elm-collage/compare/1.0.2...1.1.0) (2017-10-11)

### Added

  - Collage.Layout
    - Way to calculate all four envelope distances at once using `distances`
    - Result is a `Distances` record with up, down, right, and left fields
    - `debug` function which shows the envelope _and_ the origin
  - Html example
  - More documentation

### Fixed

  - Direction of rotation, all rotations are now counter clockwise
  - New algorithm to calculate envelopes using distances
    - Fix bug that handled ellipses incorrectly
    - Fix bug that handled groups incorrectly
  - Small optimisation in envelope calculation


## [1.0.2](https://github.com/timjs/elm-collage/compare/1.0.1...1.0.2) (2017-10-10)

### Added
  - More documentation


## [1.0.1](https://github.com/timjs/elm-collage/compare/1.0.0...1.0.1) (2017-10-10)

### Added

  - More documentation


## 1.0.0 (2017-10-10)

Initial release!
