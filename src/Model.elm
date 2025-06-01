module Model exposing (..)

type alias Model =
    { script : String
    , modifier : Bool
    , autoLoad : Bool
    , autoRun : Bool
    , isRunning : Bool
    , tickMs : Float
    , showParseResult : Bool
    , arena : String
    , world : World
    }

type Msg = UpdateScript String
    | StoreScript
    | AutoLoad Bool
    | AutoRun Bool
    | ToggleProcess Bool
    | RunPause
    | RunStep Bool
    | ModifierDown KeyEvent
    | ModifierUp KeyEvent
    | SetTicks Float
    | SetArena String
    | NOOP

type alias KeyEvent =
    { key : String
    , ctrl : Bool
    , alt : Bool
    , shift : Bool
    , meta : Bool
    }

-- Basis-IDs
type alias BotId     = Int
type alias Tick      = Int
type alias Coord     = (Int, Int)
type TurnDir
    = RIGHT     -- +90 Grad
    | LEFT      -- -90 Grad
    | AROUND    -- 180 Grad (Umdrehen)
    | STRAIGHT  -- Geradeaus (0 Grad, nicht drehen)

-- Instruktionen, die der Parser aus der DSL erzeugt
type Instr
    = Move Int           -- Felder vorwärts
    | Turn TurnDir       -- Grad (+ = rechts)
    | Scan               -- Feld-of-View -> Event
    | Fire               -- Rel. Range and Distance to enemy
    | NoOp
    | Repeat Int Instr
    | IfThenElse Cond Instr Instr
    | While Cond Instr

type Cond
    = EnemyAhead
    | WallAhead
    | LowHp
    | Always
    | Not Cond

-- Ein einzelner Bot im Spielzustand
type alias BotEntity =
    { id         : BotId
    , name       : String
    , pos        : Coord
    , dirDeg     : Int               -- 0 = Norden
    , hp         : Int
    , range      : Int
    , program    : List Instr       -- Ergebnis des Parsers
    , pc         : Int              -- Program-Counter
    , alive      : Bool
    , fireAt     : List Coord
    , viewEnv    : List (Coord, Obj)
    }

-- Walls, Power-Ups usw. lassen sich später als Objekt-Union anhängen
type Obj
    = Wall Coord                     -- blocked Coord
    | HealPack Coord Int             -- +HP
    | Bot BotEntity                  -- Bot-Objekt (für Scan)

-- Konfiguration des Schlachtfelds (vor Start einlesbar aus JSON/YAML)
type alias ArenaConfig =
    { size        : Coord            -- (width , height)
    , goAround    : Bool             -- Can go from 0,0 to Max,Max Coord
    , maxHp       : Int
    , seed        : Int              -- Spawns (objects generator)
    , objects     : List Obj         -- or Hardcoded objects
    }

-- Globaler Spielzustand
type alias World =
    { tick        : Tick
    , arena       : ArenaConfig
    , bots        : List BotEntity
    , error       : Maybe String
    , queue       : List Int      -- FIFO für interpretierte Instruktionen
    }