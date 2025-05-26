module Engine exposing (tick)

import Model exposing (World, Bot, Instr(..))

scanEnvironment : World -> Bot -> a
scanEnvironment w b =
    -- TODO: Implement environment scanning 
    Debug.todo "scanEnvironment not implemented"

runBot : World -> Bot -> Bot
runBot w b = case (List.drop b.pc b.program) of
    (Move n :: _) -> { b | pc = b.pc + 1, pos = (n + Tuple.first b.pos, Tuple.second b.pos) }
    (Turn n :: _) -> { b | pc = b.pc + 1, dirDeg = modBy 360 (b.dirDeg + n) }
    (Scan :: _) -> { b | pc = b.pc + 1, viewEnv = scanEnvironment w b }    -- Scan environment, view angle is 90 degrees -> 45 degrees left and right
    (Fire dx dy :: _) ->
        let newPos = (Tuple.first b.pos + dx, Tuple.second b.pos + dy)
        in
        if List.any (\bot -> bot.pos == newPos && bot.alive) w.bots then
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