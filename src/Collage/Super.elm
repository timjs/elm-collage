module Collage.Super exposing (BasicCollage)

import Collage
import Collage.Core as Core
import Collage.Text as Text


{-| Used at the core of a collage.
Only for internal usage.
-}
type alias BasicCollage msg =
    Core.BasicCollage Core.FillStyle Collage.LineStyle Text.Style msg
