module Main exposing (main)

import Browser
import Html exposing (Html, div, table, tr, td, text)
import Html.Attributes exposing (style)
import Model exposing (World, Bot, Obj(..), Coord)

-- MODEL

type alias Model = World
type Cell = B Bot | O Obj | Empty

init : Model
init = { 
    tick = 0, 
    queue = [],
    bots = [
        {
            id = 1,
            name = "Foo",
            pos = (0, 0),
            dirDeg = 180,
            hp = 10,
            program = [],
            pc = 0,
            alive = True
        }
    ],
    arena = {
        size = (10, 10),
        goAround = False,
        maxHp = 10,
        seed = 0,
        objects = [Wall (2, 2)]
        }
    }

-- VIEW

getObj : List Obj -> Coord -> Maybe Obj
getObj os c = case os of
    [] -> Nothing
    (o :: oss) -> case o of
        Wall cc -> if c == cc then Just o else getObj oss c
        _ -> getObj oss c
        
cell : Model -> Coord -> Cell
cell model coord = case (getObj model.arena.objects coord) of
    Just o -> O o
    Nothing -> Empty

renderCell : Model -> Coord -> Html msg
renderCell model coord =
    let content = case cell model coord of
                Empty -> "."
                O o  -> "🧱"
                _ -> "?"
    in
    td
        [ style "padding" "10px"
        , style "text-align" "center"
        , style "border" "1px solid black"
        , style "width" "40px"
        , style "height" "40px"
        ]
        [ text content ]


renderRow : Model -> Int -> Html msg
renderRow model row =
    tr [] ((List.range 0 (Tuple.second model.arena.size))
        |> List.map (\i -> (row, i))
        |> List.map (renderCell model))

view : Model -> Html msg
view model =
    div []
        [ table
            [ style "border-collapse" "collapse"
            , style "margin" "auto"
            ]
            ((List.range 0 (Tuple.second model.arena.size))
                |> List.map (renderRow model))
        ]

-- MAIN

main : Program () Model msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = \_ model -> model
        }
