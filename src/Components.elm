module Components exposing (..)

import Debug exposing (toString)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, type_, value)
import Html.Styled.Events exposing (onInput, onCheck)
import Css exposing (..)
import Styles exposing(..)

btn : List (Attribute msg) -> List (Html msg) -> Html msg
btn = styled button
    [ active [ insetBorder,  textShadow4 (px 2) (px 3) (px 3) (rgba 50 50 50 0.7)]
    , outsetBorder
    , textShadow3 (px 1) (px 1) (rgba 50 50 50 0.4)
    , fontFamily monospace
    , fontSize (Css.em 1.5)
    , padding2 (px 6) (px 4)
     ]

setting : StyledElement msg
setting = styled label [ displayGrid, property "grid-template-columns" "20ch auto", property "gap" "4px" ]

checkbox : String -> Bool -> (Bool -> msg) -> Html msg
checkbox t c u = setting []
    [ text t
    , input [ type_ "checkbox", onCheck u, Html.Styled.Attributes.checked c ] []
    ]

select : String -> String -> (String -> msg) -> List (String, String) -> Html msg
select t v u options =  setting []
    [ text t
    , Html.Styled.select
        [ onInput u 
        , css 
            [ margin2 (px 4) (px 0)
            , padding2 (px 4) (px 6)
            ]
        ]
        (List.map (\(a, b) -> (option [value a, Html.Styled.Attributes.selected (a == v)] [text b])) options)
    ]

range : String -> String -> (String -> msg) -> ( a, b, c ) -> Html msg
range t c u (min, max, step) = setting []
    [ text t
    , input
        [ type_ "range"
        , value c
        , onInput u
        , Html.Styled.Attributes.min (toString min)
        , Html.Styled.Attributes.max (toString max)
        , Html.Styled.Attributes.step (toString step)
        ] []
    ]

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

