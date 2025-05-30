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

renderWall : Html msg
renderWall = div
    [ css
    [ backgroundColor theme.wall
    , property "background-image" brickPattern
    , size (pct 100)
    , backgroundSize2 (px 20) (px 35)
    , opacity (num 0.95)
    , property "background-position" "0 0, 0 0, 10px 18px, 10px 18px, 0 0, 10px 18px"
    ]] []

healthBarBase : Color -> Float -> Html msg
healthBarBase col w = div
    [ css [ position absolute
    , border3 (px 1) groove transparent
    , width (pct 100)
    , height (px 3)
    , top (px -2)
    , boxSizing borderBox
    , backgroundColor col
    , width (pct w)
    ]]
    []

pointer : Float -> Html msg
pointer angle = div [ css
    [ position absolute
    , zIndex (int 0)
    , border3 (px 16) groove transparent
    , borderRightColor theme.directionPointer
    , borderTopColor theme.directionPointer
    , margin (px 0)
    , transforms [rotateZ (deg (angle - 90)), translateX (px 16), scale 0.3, rotateZ (deg 45)]
    , size (px 0)
    , boxSizing borderBox
    , left (calc (pct 50) minus (px 16))
    ]] []

brickPattern : String
brickPattern = """
    linear-gradient(30deg, #c9751b 12%, transparent 12.5%, transparent 87%, #c9751b 87.5%, #c9751b),
    linear-gradient(150deg, #c9751b 12%, transparent 12.5%, transparent 87%, #c9751b 87.5%, #c9751b),
    linear-gradient(30deg, #c9751b 12%, transparent 12.5%, transparent 87%, #c9751b 87.5%, #c9751b),
    linear-gradient(150deg, #c9751b 12%, transparent 12.5%, transparent 87%, #c9751b 87.5%, #c9751b),
    linear-gradient(60deg, #c9751b77 25%, transparent 25.5%, transparent 75%, #c9751b77 75%, #c9751b77),
    linear-gradient(60deg, #c9751b77 25%, transparent 25.5%, transparent 75%, #c9751b77 75%, #c9751b77)
"""

actionRow : StyledElement msg
actionRow = styled div [ grid, property "grid" "auto-flow / repeat(auto-fit, minmax(18ch, 1fr))" ]

grid : Style
grid = Css.batch [ property "display" "grid", property "gap" "12px", property "grid-template-rows" "1fr auto" ]

size : LengthOrAuto c -> Style
size c = Css.batch [ width c, height c ]

bigBorder : Style
bigBorder  = border3 (px 6) outset theme.gridLines

insetBorder : Style
insetBorder = Css.batch [ bigBorder, borderStyle inset ]

outsetBorder : Style
outsetBorder = Css.batch [ bigBorder, borderStyle outset ]

btn : List (Attribute msg) -> List (Html msg) -> Html msg
btn = styled button [ outsetBorder, fontFamily monospace, fontSize (Css.em 1.5), padding2 (px 6) (px 4) ]
