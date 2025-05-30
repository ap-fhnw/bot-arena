module Main exposing (main)

import Browser

import Model exposing (..)
import Parse exposing (parseBotScript)
import Engine exposing (tick)
import View exposing (renderView, subscriptions)

-- MODEL

init : () -> (Model, Cmd Msg)
init _ =
    ({ script = ""
    , modifier = False
    , world =
        { tick = 0
        , queue = []
        , bots = [
            { id = 1
            , name = "Foo"
            , pos = (0, 0)
            , dirDeg = 180
            , hp = 10
            , program = []
            , pc = 0
            , alive = True
            , viewEnv = []
            }, 
            { id = 2
            , name = "Bar"
            , pos = (5, 5)
            , dirDeg = -90
            , hp = 10
            , program = []
            , pc = 0
            , alive = True
            , viewEnv = []
            }]
        , arena =
            { size = (8, 8)
            , goAround = False
            , maxHp = 10
            , seed = 0
            , objects = [ Wall (2, 2), Wall (2, 3) ]
            }
        }
    }, Cmd.none)

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    StoreScript -> ({ model | world = loadScript model }, Cmd.none)
    UpdateScript content -> ({ model | script = content }, Cmd.none)
    RunStep -> ({ model | world = tick model.world }, Cmd.none)
    ModifierDown k -> ({ model
        | modifier = True 
        , world = if k.key == "Enter" then loadScript model else model.world
        }, Cmd.none)
    ModifierUp _ -> ({ model | modifier = False }, Cmd.none)
    NOOP -> (model, Cmd.none)

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
        },
        { id = 2
        , name = "Bar"
        , pos = (5, 5)
        , dirDeg = 0
        , hp = 10
        , program = parseBotScript """
            MOVE 1
            TURN 90
            FIRE 0 0
            MOVE 1
            TURN 90
            FIRE 0 1 """
        , pc = 0
        , alive = True
        , viewEnv = []
        }
        ]
    }

-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = renderView
        , update = update
        , subscriptions = subscriptions
        }
