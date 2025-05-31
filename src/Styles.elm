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
    , wallBorder : Color
    , gridLines : Color
    , debugBg : Color
    }
theme =
    { healthBarBackground = rgb 0 0 0
    , healthBarForeground = rgb 14 228 57
    , directionPointer = rgb 255 0 0
    , wall = rgb 120 100 80
    , wallBorder = rgb 90 70 20
    , gridLines = rgba 100 100 100 0.7
    , debugBg = rgba 120 120 120 0.3
    }

renderWall : Html msg
renderWall = div
    [ css
        [ backgroundColor theme.wall
        , borderColor theme.wallBorder
        , outsetBorder
        , size (pct 100)
        ]
    ] []

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

actionRow : StyledElement msg
actionRow = styled div [ grid, property "grid" "auto-flow / repeat(auto-fit, minmax(18ch, 1fr))" ]

setting : StyledElement msg
setting = styled label [ grid, property "grid-template-columns" "20ch auto", property "gap" "4px" ]

grid : Style
grid = Css.batch [ property "display" "grid", property "gap" "12px", property "grid-template-rows" "1fr auto" ]

size : LengthOrAuto c -> Style
size c = Css.batch [ width c, height c ]

bigBorder : Style
bigBorder  = border3 (px 6) outset theme.gridLines

insetBorder : Style
insetBorder = Css.batch [ bigBorder, borderStyle inset ]

outsetBorder : Style
outsetBorder = Css.batch [ bigBorder, borderStyle outset, boxSizing borderBox ]

btn : List (Attribute msg) -> List (Html msg) -> Html msg
btn = styled button
    [ active [ insetBorder,  textShadow4 (px 2) (px 3) (px 3) (rgba 50 50 50 0.7)]
    , outsetBorder
    , textShadow3 (px 1) (px 1) (rgba 50 50 50 0.4)
    , fontFamily monospace
    , fontSize (Css.em 1.5)
    , padding2 (px 6) (px 4)
     ]
