module Main exposing (main)

import Browser
import Html exposing (Html)

import Model exposing (..)
import Parse exposing (parseBotScript)
import Engine exposing (tick)
import View exposing (renderView)

-- MODEL

init : Model
init =
    { script = ""
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
            } ]
        , arena =
            { size = (8, 8)
            , goAround = False
            , maxHp = 10
            , seed = 0
            , objects = [ Wall (2, 2), Wall (2, 3) ]
            }
        }
    }

-- VIEW

view : Model -> Html Msg
view = renderView

-- UPDATE

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
