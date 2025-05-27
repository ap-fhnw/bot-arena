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

renderCell : World -> Coord -> Html msg
renderCell model coord =
    let content = case cell model coord of
                    Empty -> text ""
                    O o  -> div
                        [ style "background" "rgb(163, 86, 23)"
                        , style "width" "100%"
                        , style "height" "100%"
                        ] [ text ""]
                    B b -> div
                        [ style "transform" "rotateZ(0deg)"
                        , style "font-size" "1.8em"
                        , style "width" "100%"
                        , style "height" "100%"
                        , style "display" "grid"
                        , style "align-items" "center"
                        ]
                        [ div
                            [ style "background" "transparent"
                            , style "position" "absolute"
                            , style "z-index" "-1"
                            , style "border" "groove 16px transparent"
                            , style "border-right" "groove 16px red"
                            , style "border-top" "groove 16px red"
                            , style "margin" ".2em"
                            , style "transform" ("rotateZ(" ++ (toString (b.dirDeg - 90)) ++ "deg) translateX(19px) scale(0.35, 0.35) rotateZ(45deg)")
                            , style "width" "10px"
                            , style "height" "10px"
                            ] []
                        , text "🤖"
                        ]
    in
    td
        [ style "padding" "1px"
        , style "text-align" "center"
        , style "border" "1px solid rgba(0, 0, 0, 0.38)"
        , style "width" "50px"
        , style "height" "50px"
        ]
        [ content ]

renderRow : World -> Int -> Html msg
renderRow world row =
    tr [] ((List.range 0 (Tuple.second world.arena.size))
        |> List.map (\i -> (row, i))
        |> List.map (renderCell world))


showInstruction : Instr -> String
showInstruction i = case i of
    Move n -> "MOVE " ++ toString n
    _ -> "?"

showProgram : Int -> List Instr -> String
showProgram pc is = is
    |> List.indexedMap (\i a -> showInstruction a ++ (if i == pc then " <-" else ""))
    |> List.foldr (\a b -> a ++ "\n" ++ b) ""

mainLayout : List (Html.Attribute msg)
mainLayout =
    [ style "display" "grid"
    , style "grid-template" """
        'editor map' 16em
        'debug map' auto
        / 1fr 1fr
        """
    , style "grid-template-columns" "1fr auto"
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
            ] (List.map (\b -> text (showProgram b.pc b.program)) model.world.bots)  
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