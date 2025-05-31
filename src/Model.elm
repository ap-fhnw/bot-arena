module Model exposing (..)

type alias Model =
    { script : String
    , modifier: Bool
    , autoLoad : Bool
    , autoRun : Bool
    , isRunning : Bool
    , tickMs : Float
    , showParseResult : Bool
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

-- Instruktionen, die der Parser aus der DSL erzeugt
type Instr
    = Move Int              -- Felder vorwärts
    | Turn Int              -- Grad (+ = rechts)
    | Scan                  -- Feld-of-View -> Event
    | Fire Int Int          -- Rel. Koord.
    | NoOp
    | Repeat Int Instr
    | IfThenElse Cond Instr Instr
    | While Cond Instr

type Cond
    = EnemyAhead
    | WallAhead
    | LowHp
    | Not Cond

-- Ein einzelner Bot im Spielzustand
type alias BotEntity =
    { id         : BotId
    , name       : String
    , pos        : Coord
    , dirDeg     : Int               -- 0 = Norden
    , hp         : Int
    , program    : List Instr       -- Ergebnis des Parsers
    , pc         : Int              -- Program-Counter
    , alive      : Bool
    , fireAt     : Maybe Coord  
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
    , queue       : List Int      -- FIFO für interpretierte Instruktionen
    }