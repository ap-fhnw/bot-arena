module Styles exposing (..)

import Debug exposing (toString)
import Html.Styled exposing (..)
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

section : StyledElement msg
section = styled div
    [ padding (Css.em 1)
    , backgroundColor theme.debugBg
    , whiteSpace preWrap, fontFamily monospace
    , fontSize (Css.em 1.5)
    , outsetBorder
    ]

actionRow : StyledElement msg
actionRow = styled div [ displayGrid, property "grid" "auto-flow / repeat(auto-fit, minmax(18ch, 1fr))" ]

displayGrid : Style
displayGrid = batch [ property "display" "grid", property "gap" "12px", gridTemplateRows "1fr auto" ]

grid : StyledElement msg
grid = styled div [ displayGrid ]

gridR : String -> StyledElement msg
gridR rows = styled div [ displayGrid, gridTemplateRows rows ]

gridC : String -> StyledElement msg
gridC cols = styled div [ displayGrid, gridTemplateCols cols ]

gridRC : String -> String -> StyledElement msg
gridRC rows cols = styled div [ displayGrid, gridTemplateRows rows, gridTemplateCols cols ]

gap : Length c u -> Style
gap l = property "gap" (toString l)

gridTemplateRows : String -> Style
gridTemplateRows rows = property "grid-template-rows" rows

gridTemplateCols : String -> Style
gridTemplateCols cols = property "grid-template-columns" cols

gridArea : String -> Style
gridArea area = property "grid-area" area

size : LengthOrAuto c -> Style
size c = batch [ width c, height c ]

largeBorder : Style
largeBorder  = border3 (px 6) outset theme.gridLines

insetBorder : Style
insetBorder = batch [ largeBorder, borderStyle inset ]

outsetBorder : Style
outsetBorder = batch [ largeBorder, borderStyle outset, boxSizing borderBox ]

scroll : Style
scroll = batch [ height (pct 100) , overflow Css.scroll ]