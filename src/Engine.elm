module Engine exposing (tick)

import Model exposing (World, Bot, Instr(..))

scanEnvironment : World -> Bot -> a
scanEnvironment w b =
    -- TODO: Implement environment scanning 
    Debug.todo "scanEnvironment not implemented"


runBot : World -> Bot -> Bot
runBot w b = case (List.drop b.pc b.program) of
    -- Move as long as there is no wall or object in the way
    (Move n :: _) -> { b | pc = b.pc + 1, pos = case b.dirDeg of
         0 -> (Tuple.first b.pos - n, Tuple.second b.pos) -- Move up
         90 -> (Tuple.first b.pos, Tuple.second b.pos + n) -- Move right
         180 -> (Tuple.first b.pos + n, Tuple.second b.pos) -- Move down
         270 -> (Tuple.first b.pos, Tuple.second b.pos - n) -- Move left
         _ -> (Tuple.first b.pos, Tuple.second b.pos) } 
    -- Turn 0, 90, 180, 270 degrees (up, right, down, left)
    (Turn n :: _) -> { b | pc = b.pc + 1, dirDeg = modBy 360 (b.dirDeg + n) }
    -- Scan environment, view angle is 90 degrees -> 45 degrees left and right
    (Scan :: _)   -> { b | pc = b.pc + 1, viewEnv = scanEnvironment w b }    
    -- Fire at coordinate
    (Fire x y :: _) -> if List.any (\bot -> bot.pos == (x, y) && bot.alive) w.bots then
                           { b | pc = b.pc + 1, alive = False } -- Bot hit, mark as dead
                       else
                           { b | pc = b.pc + 1 } -- No hit, just continue
    _ -> { b | pc = b.pc + 1 } -- No instruction or end of program

run : World -> List Bot
run w = List.map (\b -> runBot w b) w.bots

tick : World -> World
tick world = { world
    | tick = world.tick + 1
    , bots = run world
    }