module View exposing (showProgram, renderRow)
import Debug exposing (toString)
import Html exposing (Html, main_, div, table, tr, td, text, textarea, button)
import Html.Attributes exposing (style, type_, rows, attribute)
import Html.Events exposing (onInput, onClick)
import Model exposing (..)

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
                Empty -> "."
                O o  -> "🧱"
                B b -> "🤖"
    in
    td
        [ style "padding" "10px"
        , style "text-align" "center"
        , style "border" "1px solid black"
        , style "width" "40px"
        , style "height" "40px"
        ]
        [ text content ]

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