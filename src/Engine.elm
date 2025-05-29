module Engine exposing (tick)

import Model exposing (World, Bot, Instr(..), Obj(..))
import Html.Attributes exposing (coords)

scanEnvironment : World -> Bot -> a
scanEnvironment w b =
    -- TODO: Implement environment scanning 
    Debug.todo "scanEnvironment not implemented"


runBot : World -> Bot -> Bot
runBot w b = case (List.drop b.pc b.program) of
    -- Move as long as there is no wall or object in the way
    (Move n :: _) -> 
        let  
            -- Get step direction based on bot orientation
            (dx, dy) = case b.dirDeg of
                0   -> (-1, 0)  -- Move up
                90  -> (0, 1)   -- Move right
                180 -> (1, 0)   -- Move down
                270 -> (0, -1)  -- Move left
                _   -> (0, 0)   -- Don't move 
            
            -- Check if position is valid (within bounds, no wall, no bot)
            isValidPos (x, y) = 
                x >= 0 && x < Tuple.second(w.arena.size) && y >= 0 && y < Tuple.first(w.arena.size) &&
                not (List.any (\obj ->
                    case obj of
                        Wall coord -> coord == (x, y)
                        _ -> False) w.arena.objects) &&

                not (List.any (\bot -> bot.id /= b.id && bot.alive && bot.pos == (x, y)) w.bots)

            -- Try to move step by step
            moveStepByStep steps (x, y) = 
                if steps <= 0 then
                    (x, y) -- reached target
                else
                    let
                        nextPos = (x + dx, y + dy)
                    in
                    if isValidPos nextPos then
                        moveStepByStep (steps - 1) nextPos
                    else
                        (x, y)
            -- calculate the new position
            newPos = moveStepByStep n b.pos
        in 
        { b | pc = b.pc + 1, pos = newPos }

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