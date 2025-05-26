module Main exposing (main)

import Browser
import Html exposing (Html, main_, div, table, text, textarea, button)
import Html.Attributes exposing (style, type_, rows, attribute)
import Html.Events exposing (onInput, onClick)

import Model exposing (World, Obj(..), Instr(..))
import Parse exposing (parseBotScript)
import Engine exposing (tick)
import View exposing (renderRow, showProgram)

-- MODEL

type alias Model = { script : String, world : World }

init : Model
init =
    { script = ""
    , world =
        { tick = 0
        , queue = []
        , bots = []
        , arena =
            { size = (10, 10)
            , goAround = False
            , maxHp = 10
            , seed = 0
            , objects = [ Wall (2, 2), Wall (2, 3) ]
            }
        }
    }

-- VIEW

view : Model -> Html Msg
view model = main_
    [ style "display" "grid"
    , style "gap" ".5em"
    , style "padding" "1em"
    ]
    [ textarea
        [ onInput UpdateScript
        , rows 10
        , attribute "cols" "50"
        ] []
    , button
        [ onClick StoreScript
        , type_ "submit"
        ] [ text "Store" ]
    , Html.pre
        [] [ text model.script ]
    , Html.pre
        [] (List.map (\b -> text (showProgram b.pc b.program)) model.world.bots)  
    , div []
        [ table
            [ style "border-collapse" "collapse"
            , style "margin" "auto"
            ]
            ((List.range 0 (Tuple.second model.world.arena.size))
                |> List.map (renderRow model.world))
        ]
    , div []
        [ button [ onClick RunStep ] [ text "Run Step" ]
        ]
    ]

-- UPDATE

type Msg = UpdateScript String
    | StoreScript
    | RunStep

update : Msg -> Model -> Model
update msg model = case msg of
    StoreScript -> { model | world = loadScript model }
    UpdateScript content -> { model | script = content }
    RunStep -> { model | world = tick model.world }

loadScript : Model -> World
loadScript { world, script } =
    { world | bots = [
        { id = 1
        , name = "Foo"
        , pos = (0, 0)
        , dirDeg = 180
        , hp = 10
        , program = parseBotScript script
        , pc = 0
        , alive = True
        , viewEnv = []
        }
        ]
    }

-- MAIN

main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
