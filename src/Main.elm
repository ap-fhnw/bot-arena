module Main exposing (main)

import Browser

import Model exposing (..)
import Parse exposing (parseBotScript)
import Engine exposing (tick)
import View exposing (renderView, subscriptions)
import Task

-- MODEL

init : () -> (Model, Cmd Msg)
init _ =
    ({ script = ""
    , modifier = False
    , autoLoad = True
    , autoRun = True
    , isRunning = False
    , tickMs = 1000
    , showParseResult = True
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
            , fireAt = Nothing
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
            , fireAt = Nothing
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
    StoreScript -> ({ model | world = loadScript model },
        if model.autoRun && not model.isRunning then Task.perform (\_ -> RunPause) (Task.succeed 0)
        else (if not model.autoRun && model.isRunning then Task.perform (\_ -> RunPause) (Task.succeed 0)
        else Cmd.none))
    UpdateScript content -> ({ model | script = content },
        if model.autoLoad then Task.perform (\_ -> StoreScript) (Task.succeed 0) else Cmd.none)
    RunStep c -> ({ model | isRunning = c, world = tick model.world }, Cmd.none)
    RunPause -> ({ model | isRunning = not model.isRunning }, Cmd.none)
    AutoLoad b -> ({ model | autoLoad = b },
        if b then Task.perform (\_ -> StoreScript) (Task.succeed 0) else Cmd.none)
    AutoRun b -> ({ model | autoRun = b, isRunning = model.isRunning || b }, Cmd.none)
    ModifierDown k -> ({ model | modifier = True },
        if k.key == "Enter" then Task.perform (\_ -> StoreScript) (Task.succeed True) else Cmd.none)
    ModifierUp _ -> ({ model | modifier = False }, Cmd.none)
    NOOP -> (model, Cmd.none)
    SetTicks ms -> ({ model | tickMs = ms }, Cmd.none)
    ToggleProcess b -> ({ model | showParseResult = b }, Cmd.none)

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
        , fireAt = Nothing
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
        , fireAt = Nothing
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
