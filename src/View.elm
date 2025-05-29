module View exposing (renderView)

import Debug exposing (toString)
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick, onInput)
import Model exposing (..)

type Cell = B BotEntity | O Obj | Empty

getObj : List Obj -> Coord -> Maybe Obj
getObj os c = case os of
    [] -> Maybe.Nothing
    (o :: oss) -> case o of
        Wall cc -> if c == cc then Just o else getObj oss c
        _ -> getObj oss c

getBot : List BotEntity -> Coord -> Maybe BotEntity
getBot bs c = case bs of
    [] -> Maybe.Nothing
    (b :: bss) -> if b.pos == c then Just b else getBot bss c

cell : World -> Coord -> Cell
cell world coord = case (getObj world.arena.objects coord) of
    Just o -> O o
    Maybe.Nothing -> case (getBot world.bots coord) of
        Just b -> B b
        _ -> Empty

brickPattern : String
brickPattern = """
    linear-gradient(30deg, #c9751b 12%, transparent 12.5%, transparent 87%, #c9751b 87.5%, #c9751b),
    linear-gradient(150deg, #c9751b 12%, transparent 12.5%, transparent 87%, #c9751b 87.5%, #c9751b),
    linear-gradient(30deg, #c9751b 12%, transparent 12.5%, transparent 87%, #c9751b 87.5%, #c9751b),
    linear-gradient(150deg, #c9751b 12%, transparent 12.5%, transparent 87%, #c9751b 87.5%, #c9751b),
    linear-gradient(60deg, #c9751b77 25%, transparent 25.5%, transparent 75%, #c9751b77 75%, #c9751b77),
    linear-gradient(60deg, #c9751b77 25%, transparent 25.5%, transparent 75%, #c9751b77 75%, #c9751b77)
"""

theme :
    { healthBarBackground : Color
    , healthBarForeground : Color
    }
theme =
    { healthBarBackground = rgb 0 0 0
    , healthBarForeground = rgb 14 228 57
    }

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

renderBot : Bot -> World -> Html Msg
renderBot bot w = div
    [ css [ transform (translateY (px 2))
    , fontSize (Css.em 1.8)
    , width (pct 100)
    , height (pct 100)
    , property "display" "grid"
    , alignItems center
    ]]
    [ div
        [css [ property "background" "transparent"
        , property "position" "absolute"
        , property "z-index" "0"
        , property "border" "groove 16px transparent"
        , property "border-right" "groove 16px red"
        , property "border-top" "groove 16px red"
        , property "margin" "0"
        , property "transform" ("rotateZ(" ++ (toString (bot.dirDeg - 90)) ++ "deg) translateX(16px) scale(0.3, 0.3) rotateZ(45deg)  ")
        , property "width" "0px"
        , property "height" "0px"
        , property "box-sizing" "border-box"
        , property "left" "calc(50% - 16px)"
        ]] []
    , healthBarBase theme.healthBarBackground 100
    , healthBarBase theme.healthBarForeground (100 * toFloat bot.hp / toFloat w.arena.maxHp)
    , text (if bot.alive then "🤖" else "🪦")
    ]

renderCell : World -> Coord -> Html Msg
renderCell model coord =
    let content = case cell model coord of
                    Empty -> text ""
                    O _ -> div
                        [css [ property "background" "rgb(120, 61, 14)"
                        , property "background-image" brickPattern
                        , property "width" "100%"
                        , property "height" "100%"
                        , property "background-size" "20px 35px"
                        , property "background-position" "0 0, 0 0, 10px 18px, 10px 18px, 0 0, 10px 18px"
                        ]] [ text ""]
                    B b -> renderBot b model
    in
    td
        [css[ property "padding" "1px"
        , property "text-align" "center"
        , property "border" "1px solid rgba(0, 0, 0, 0.38)"
        , property "width" "50px"
        , property "height" "50px"
        ]]
        [ content ]

renderRow : World -> Int -> Html Msg
renderRow world row =
    tr [] ((List.range 0 (Tuple.second world.arena.size))
        |> List.map (\i -> (row, i))
        |> List.map (renderCell world))


showInstruction : Instr -> String
showInstruction i = case i of
    Move n -> "MOVE " ++ toString n
    Turn n -> "TURN " ++ toString n ++ "°"
    Scan -> "SCAN"
    Fire x y -> "FIRE " ++ toString x ++ " " ++ toString y
    Repeat n instr -> "REPEAT " ++ toString n ++ " " ++ showInstruction instr
    IfThenElse cond i1 i2 -> "IF " ++ showCond cond ++ " THEN " ++ showInstruction i1 ++ " ELSE " ++ showInstruction i2
    While cond instr -> "WHILE " ++ showCond cond ++ " DO " ++ showInstruction instr
    _ -> "?"

showCond : Cond -> String
showCond c = case c of
    EnemyAhead -> "ENEMY-AHEAD"
    LowHp -> "LOW-HP"
    WallAhead -> "WALL-AHEAD"
    Not inner -> "NOT " ++ showCond inner

showProgram : Int -> List Instr -> String
showProgram pc is = is
    |> List.indexedMap (\i a -> showInstruction a ++ (if i == pc then " <-" else ""))
    |> List.foldr (\a b -> a ++ "\n" ++ b) ""

mainLayout : List (Html.Styled.Attribute msg)
mainLayout =
    [ css [ property "display" "grid"
    , property "grid-template" """
        'map' auto
        'editor' auto
        'debug' auto
        / 1fr 1fr
        """
    , property "grid-template-columns" "1fr"
    , property "gap" ".5em"
    , property "padding" "2em"
    ] ]

renderView : Model -> Html.Html Msg
renderView model = toUnstyled (main_ mainLayout
    [ div
        [ css [ property "grid-area" "editor"
        , property "display" "grid"
        , property "grid-template-rows" "1fr auto"
        , property "gap" ".5em"
        ]]
        [ textarea
            [ onInput UpdateScript
            , Html.Styled.Attributes.spellcheck False
            , css [property "resize" "none"
            , property "font-size" "1.5em"
            , property "padding" "8px"
            , property "text-transform" "uppercase"]
            ] []
        , button
            [ onClick StoreScript
            , css [property "justify-self" "center"
            , property "font-size" "1.5em"]
            ] [ text "Store" ]
        ]
    , div [ css [property "grid-area" "debug"
          , property "font-size" "1.5em"]
        ]
        [ Html.Styled.pre
            [ css [ property "padding" "1em"
            , property "background" "whitesmoke" ]
            ] (List.map (\b -> text (b.name ++ "\n" ++ (showProgram b.pc b.program))) model.world.bots)  
        ]
    , div
        [ css [property "grid-area" "map"
        , property "display" "flex"
        , property "flex-direction" "column"
        , property "align-items" "center"
        , property "gap" ".5em"]
        ]
        [ Html.Styled.table
            [ css [property "border-collapse" "collapse"
            , property "margin" "auto"]
            ]
            ((List.range 0 (Tuple.second model.world.arena.size))
                |> List.map (renderRow model.world))
        , div []
            [ button [ onClick RunStep            , css [property "font-size" "1.5em"]
                     ] [ text "Run Step" ]
            ]
        ]
    ])
