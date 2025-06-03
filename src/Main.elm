module Main exposing (main, unpackScript)

import Browser

import Model exposing (..)
import Parse exposing (parseBotScript, parseBotScriptSave)
import Engine exposing (tick)
import View exposing (renderView, subscriptions)
import Task

-- MODEL

createBot : Int -> String -> Coord -> Int -> List Instr -> BotEntity
createBot id name pos dir pgr =
    { id = id
    , name = name
    , pos = pos
    , dirDeg = dir
    , hp = 10
    , range = 5
    , program = pgr
    , pc = 0
    , execStack = []
    , alive = True
    , viewEnv = [] 
    , fireAt = []
    }

init : () -> (Model, Cmd Msg)
init _ = (
    { script = ""
    , modifier = False
    , autoLoad = True
    , autoRun = False
    , isRunning = False
    , tickMs = 600
    , showParseResult = True
    , arena = "prison"
    , world =
        { tick = 0
        , queue = []
        , error = Nothing
        , bots = 
            [ createBot 1 "Foo" (4, 2) 90 []
            , createBot 2 "Bar" (4, 6) -90 []
            ]
        , arena =
            { size = (8, 8)
            , goAround = False
            , maxHp = 10
            , seed = 0
            , objects = List.map Wall [ (2, 2), (2, 6), (6, 2), (6, 6) ]
            }
        }
    }
    , Cmd.none
    )

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
    SetArena option -> ({ model | arena = option }, Task.perform (\_ -> StoreScript) (Task.succeed 0))

unpackScript : Model -> (List Instr, Maybe String)
unpackScript m =
    let parsedScript = parseBotScript m.script
        previousScript = List.head m.world.bots
            |> Maybe.map .program
            |> Maybe.withDefault []
        script = parsedScript |> Result.withDefault previousScript
        err = case parsedScript of
            Err(msg) -> Just msg
            _ -> Nothing
    in (script, err)

beginnerWorld : Model -> World
beginnerWorld m = let (script, err) = unpackScript m in
    { tick = 0
    , error = err
    , queue = []
    , bots = 
        [ createBot 1 "Foo" (4, 2) 90 script
        , createBot 2 "Bar" (4, 6) -90 (parseBotScriptSave """
            MOVE 1
            TURN right
            FIRE
            MOVE 1
            TURN right
            FIRE
            """)
        ]
    , arena =
        { size = (8, 8)
        , goAround = False
        , maxHp = 10
        , seed = 0
        , objects = List.map Wall [ (2, 2), (2, 6), (6, 2), (6, 6) ]
        }
    }

prisonWorld : Model -> World
prisonWorld m = let (script, err) = unpackScript m in
    { tick = 0
    , queue = []
    , error = err
    , bots = 
        [ createBot 1 "Foo" (3, 2) 180 script
        , createBot 2 "Bar" (4, 5) 0 (parseBotScriptSave """
            while true do [
                if enemyahead
                    then [nothing fire]
                else [
                    turn left
                    move 1
                ]
            ]
            """)
        ]
    , arena =
        { size = (7, 7)
        , goAround = False
        , maxHp = 10
        , seed = 0
        , objects = List.map Wall ((List.repeat 7 [(0, 0)] |> List.indexedMap (\i _-> (0, i + 1)))
            ++ (List.repeat 7 [(0, 0)] |> List.indexedMap (\i _-> (i, 0)))
            ++ (List.repeat 7 [(0, 0)] |> List.indexedMap (\i _-> (i + 1, 7)))
            ++ (List.repeat 7 [(0, 0)] |> List.indexedMap (\i _-> (7, i))))
        }
    }

loadScript : Model -> World
loadScript m = case m.arena of
    "beginner" -> beginnerWorld m
    "prison" -> prisonWorld m
    _ -> beginnerWorld m

-- MAIN

main : Program () Model Msg
main =
    let (w, c) = init () in 
    Browser.element
        { init = (\_ -> ({ w | world = loadScript w }, c)) 
        , view = renderView
        , update = update
        , subscriptions = subscriptions
        }
