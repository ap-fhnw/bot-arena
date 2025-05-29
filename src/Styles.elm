module Styles exposing (..)

import Debug exposing (toString)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, spellcheck)
import Html.Styled.Events exposing (onClick, onInput)
import Css exposing (..)

type alias StyledElement msg = List (Attribute msg) -> List (Html msg) -> Html msg

theme :
    { healthBarBackground : Color
    , healthBarForeground : Color
    , directionPointer : Color
    , wall : Color
    , gridLines : Color
    , debugBg : Color
    }
theme =
    { healthBarBackground = rgb 0 0 0
    , healthBarForeground = rgb 14 228 57
    , directionPointer = rgb 255 0 0
    , wall = rgb 100 51 14
    , gridLines = rgba 100 100 100 0.7
    , debugBg = rgba 120 120 120 0.3
    }

actionRow : StyledElement msg
actionRow = styled div [ grid, property "grid" "auto-flow / repeat(auto-fit, minmax(32ch, 1fr))" ]

grid : Style
grid = Css.batch [ property "display" "grid", property "gap" "20px", property "grid-template-rows" "1fr auto" ]

brickPattern : String
brickPattern = """
    linear-gradient(30deg, #c9751b 12%, transparent 12.5%, transparent 87%, #c9751b 87.5%, #c9751b),
    linear-gradient(150deg, #c9751b 12%, transparent 12.5%, transparent 87%, #c9751b 87.5%, #c9751b),
    linear-gradient(30deg, #c9751b 12%, transparent 12.5%, transparent 87%, #c9751b 87.5%, #c9751b),
    linear-gradient(150deg, #c9751b 12%, transparent 12.5%, transparent 87%, #c9751b 87.5%, #c9751b),
    linear-gradient(60deg, #c9751b77 25%, transparent 25.5%, transparent 75%, #c9751b77 75%, #c9751b77),
    linear-gradient(60deg, #c9751b77 25%, transparent 25.5%, transparent 75%, #c9751b77 75%, #c9751b77)
"""

size : LengthOrAuto c -> Style
size c = Css.batch [ width c, height c ]

bigBorder  = border3 (px 6) outset theme.gridLines
insetBorder = Css.batch [ bigBorder, borderStyle inset ]
outsetBorder = Css.batch [ bigBorder, borderStyle outset ]

btn : List (Attribute msg) -> List (Html msg) -> Html msg
btn = styled button [ fontFamily monospace, fontSize (Css.em 1.5), padding (px 4) ]
