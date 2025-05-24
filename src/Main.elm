module Main exposing (main)

import Browser
import Html exposing (Html, div, table, tr, td, text)
import Html.Attributes exposing (style)
import Model exposing (World)

-- MODEL

type Cell = Empty | Bot1 | Bot2

type alias Model = World

-- Grid definition
init : Model
init = { 
    tick = 0, 
    queue = [],
    bots = [],
    arena = {
        size = (3, 10),
        goAround = False,
        maxHp = 10,
        seed = 0,
        objects = []
        }
    }

-- VIEW

renderCell : Cell -> Html msg
renderCell cell =
    let
        content =
            case cell of
                Empty -> "."
                Bot1  -> "🤖"
                Bot2  -> "💀"
    in
    td
        [ style "padding" "10px"
        , style "text-align" "center"
        , style "border" "1px solid black"
        , style "width" "40px"
        , style "height" "40px"
        ]
        [ text content ]


renderRow : List Cell -> Html msg
renderRow cells =
    tr [] (List.map renderCell cells)

view : Model -> Html msg
view model =
    div []
        [ table
            [ style "border-collapse" "collapse"
            , style "margin" "auto"
            ]
            (List.map renderRow ((List.repeat
                (Tuple.first model.arena.size)
                (List.repeat (Tuple.second model.arena.size) Empty))))
        ]

-- MAIN

main : Program () Model msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = \_ model -> model
        }
