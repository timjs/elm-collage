# Changelog


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
