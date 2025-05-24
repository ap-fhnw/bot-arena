# Design

Proposal for the data model:

```elm
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
    | Nothing

-- Ein einzelner Bot im Spielzustand
type alias Bot =
    { id         : BotId
    , name       : String
    , pos        : Coord
    , dirDeg     : Int               -- 0 = Norden
    , hp         : Int
    , program    : Array Instr       -- Ergebnis des Parsers
    , pc         : Int               -- Program-Counter
    , alive      : Bool
    }

-- Walls, Power-Ups usw. lassen sich später als Objekt-Union anhängen
type Obj
    = Wall Coord                     -- blocked Coord
    | HealPack Coord Int             -- +HP

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
    , bots        : Dict BotId Bot
    , queue       : Queue Event      -- FIFO für interpretierte Instruktionen
    }
```

Example script:

Imperative:

```
IF SCAN THEN FIRE 1 0
ELSE TURN 45 MOVE 1
```

Functional:

```
(IF (SCAN) (FIRE 0 1) (TURN 90))
```
