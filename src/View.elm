module View exposing (renderView)

import Debug exposing (toString)
import Html exposing (Html, main_, div, table, tr, td, text, textarea, button)
import Html.Attributes exposing (style, type_)
import Html.Events exposing (onInput, onClick)
import Model exposing (..)
import Html.Attributes exposing (spellcheck)

type Cell = B Bot | O Obj | Empty

getObj : List Obj -> Coord -> Maybe Obj
getObj os c = case os of
    [] -> Maybe.Nothing
    (o :: oss) -> case o of
        Wall cc -> if c == cc then Just o else getObj oss c
        _ -> getObj oss c

getBot : List Bot -> Coord -> Maybe Bot
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

renderBot : Bot -> World -> Html Msg
renderBot bot w = div
    [ style "transform" "translateY(2px)"
    , style "font-size" "1.8em"
    , style "width" "100%"
    , style "height" "100%"
    , style "display" "grid"
    , style "align-items" "center"
    ]
    [ div
        [ style "background" "transparent"
        , style "position" "absolute"
        , style "z-index" "0"
        , style "border" "groove 16px transparent"
        , style "border-right" "groove 16px red"
        , style "border-top" "groove 16px red"
        , style "margin" "0"
        , style "transform" ("rotateZ(" ++ (toString (bot.dirDeg - 90)) ++ "deg) translateX(16px) scale(0.3, 0.3) rotateZ(45deg)  ")
        , style "width" "0px"
        , style "height" "0px"
        , style "box-sizing" "border-box"
        , style "left" "calc(50% - 16px)"
        ] []
    , div
        [ style "background" "black"
        , style "position" "absolute"
        , style "z-index" "-2"
        , style "border" "groove 1px transparent"
        , style "width" "100%"
        , style "height" "3px"
        , style "top" "-2px"
        , style "box-sizing" "border-box"
        ] []
    , div
        [ style "background" "rgb(14, 228, 57)"
        , style "position" "absolute"
        , style "z-index" "-1"
        , style "border" "groove 1px transparent"
        , style "width" (toString (100 * toFloat bot.hp / toFloat w.arena.maxHp) ++ "%") 
        , style "height" "3px"
        , style "top" "-2px"
        , style "box-sizing" "border-box"
        ] []
    , text (if bot.alive then "🤖" else "🪦")
    ]

renderCell : World -> Coord -> Html Msg
renderCell model coord =
    let content = case cell model coord of
                    Empty -> text ""
                    O o  -> div
                        [ style "background" "rgb(120, 61, 14)"
                        , style "background-image" brickPattern
                        , style "width" "100%"
                        , style "height" "100%"
                        , style "background-size" "20px 35px"
                        , style "background-position" "0 0, 0 0, 10px 18px, 10px 18px, 0 0, 10px 18px"
                        ] [ text ""]
                    B b -> renderBot b model
    in
    td
        [ style "padding" "1px"
        , style "text-align" "center"
        , style "border" "1px solid rgba(0, 0, 0, 0.38)"
        , style "width" "50px"
        , style "height" "50px"
        ]
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

mainLayout : List (Html.Attribute msg)
mainLayout =
    [ style "display" "grid"
    , style "grid-template" """
        'map' auto
        'editor' auto
        'debug' auto
        / 1fr 1fr
        """
    , style "grid-template-columns" "1fr"
    , style "gap" ".5em"
    , style "padding" "2em"
    ]

renderView : Model -> Html Msg
renderView model = main_ mainLayout
    [ div
        [ style "grid-area" "editor"
        , style "display" "grid"
        , style "grid-template-rows" "1fr auto"
        , style "gap" ".5em"
        ]
        [ textarea
            [ onInput UpdateScript
            , spellcheck False
            , style "resize" "none"
            , style "font-size" "1.5em"
            , style "padding" "8px"
            , style "text-transform" "uppercase"
            ] []
        , button
            [ onClick StoreScript
            , type_ "submit"
            , style "justify-self" "center"
            , style "font-size" "1.5em"
            ] [ text "Store" ]
        ]
    , div [ style "grid-area" "debug"
          , style "font-size" "1.5em"
        ]
        [ Html.pre
            [ style "padding" "1em"
            , style "background" "whitesmoke"
            ] (List.map (\b -> text (b.name ++ "\n" ++ (showProgram b.pc b.program))) model.world.bots)  
        ]
    , div
        [ style "grid-area" "map"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "gap" ".5em"
        ]
        [ table
            [ style "border-collapse" "collapse"
            , style "margin" "auto"
            ]
            ((List.range 0 (Tuple.second model.world.arena.size))
                |> List.map (renderRow model.world))
        , div []
            [ button [ onClick RunStep            , style "font-size" "1.5em"
                     ] [ text "Run Step" ]
            ]
        ]
    ]