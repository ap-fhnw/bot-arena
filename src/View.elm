module View exposing (renderView)

import Debug exposing (toString)
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, spellcheck)
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
    , directionPointer : Color
    , wall : Color
    , gridLines : Color
    }
theme =
    { healthBarBackground = rgb 0 0 0
    , healthBarForeground = rgb 14 228 57
    , directionPointer = rgb 255 0 0
    , wall = rgb 120 61 14
    , gridLines = rgba 0 0 0 0.38
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

pointer : Float -> Html Msg
pointer angle = div [ css
    [ position absolute
    , zIndex (int 0)
    , border3 (px 16) groove transparent
    , borderRightColor theme.directionPointer
    , borderTopColor theme.directionPointer
    , margin (px 0)
    , transforms [rotateZ (deg (angle - 90)), translateX (px 16), scale 0.3, rotateZ (deg 45)]
    , width (px 0)
    , height (px 0)
    , boxSizing borderBox
    , left (calc (pct 50) minus (px 16))
    ]] []

renderBot : BotEntity -> World -> Html Msg

renderBot bot w = div
    [ css [ transform (translateY (px 2))
    , fontSize (Css.em 1.8)
    , width (pct 100)
    , height (pct 100)
    , property "display" "grid"
    , alignItems center
    ]]
    [ pointer (toFloat bot.dirDeg)
    , healthBarBase theme.healthBarBackground 100
    , healthBarBase theme.healthBarForeground (100 * toFloat bot.hp / toFloat w.arena.maxHp)
    , text (if bot.alive then "🤖" else "🪦")
    ]

renderWall : Html msg
renderWall = div
    [ css
    [ backgroundColor theme.wall
    , property "background-image" brickPattern
    , width (pct 100)
    , height (pct 100)
    , backgroundSize2 (px 20) (px 35)
    , property "background-position" "0 0, 0 0, 10px 18px, 10px 18px, 0 0, 10px 18px"
    ]] []

renderCell : World -> Coord -> Html Msg
renderCell world pos = td
    [ css
        [ padding (px 1)
        , textAlign center
        , border3 (px 1) solid theme.gridLines
        , width (px 50)
        , height (px 50)
        ]
    ]
    [ case cell world pos of
        B b -> renderBot b world
        O _ -> renderWall
        Empty -> text ""
    ]

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
mainLayout = [ css 
    [ property "display" "grid"
    , property "grid-template" """
        'map' auto
        'editor' auto
        'debug' auto
        / 1fr 1fr
        """
    , property "grid-template-columns" "1fr"
    , property "gap" ".5em"
    , padding (Css.em 2)
    ]]

renderView : Model -> Html.Html Msg
renderView model = toUnstyled (main_ mainLayout
    [ div
        [ css
        [ property "grid-area" "editor"
        , property "display" "grid"
        , property "grid-template-rows" "1fr auto"
        , property "gap" ".5em"
        ]]
        [ textarea
            [ onInput UpdateScript
            , spellcheck False
            , css
                [ property "resize" "none"
                , property "font-size" "1.5em"
                , property "padding" "8px"
                , property "text-transform" "uppercase" ]
            ] []
        , button
            [ onClick StoreScript
            , css
                [ property "justify-self" "center"
                , property "font-size" "1.5em" ]
            ] [ text "Store" ]
        ]
    , div
        [ css 
        [ property "grid-area" "debug"
        , fontSize (Css.em 1.5)
        ]]
        [ Html.Styled.pre
            [ css [ padding (Css.em 1), property "background" "whitesmoke" ]
            ] (List.map (\b -> text (b.name ++ "\n" ++ (showProgram b.pc b.program))) model.world.bots)  
        ]
    , div
        [ css
        [ property "grid-area" "map"
        , displayFlex
        , flexDirection column
        , alignItems center
        , property "gap" ".5em"
        ]]
        [ Html.Styled.table
            [ css
            [ borderCollapse collapse
            , margin auto
            ]]
            ((List.range 0 (Tuple.second model.world.arena.size))
                |> List.map (renderRow model.world))
        , div []
            [ button
                [ onClick RunStep            
                , css [fontSize (Css.em 1.5)]
                ]
                [ text "Run Step" ]
            ]
        ]
    ])
