module Engine exposing (tick)

import Model exposing (World, Bot, Instr(..), Obj(..))
import Html.Attributes exposing (coords)
import Model exposing (Cond(..))
import Dict exposing (update)

scanEnvironment : World -> Bot -> List (Model.Coord, Model.Obj) 
scanEnvironment w b =
    -- TODO: Implement environment scanning like a radar with max range
    let
        maxRadius = 4

        -- Bot position
        (botX, botY) = b.pos

        -- Check if a coordinate is within the radar radius
        isInRadarRange : Model.Coord -> Bool
        isInRadarRange (x, y) = 
            let
                dx = x - botX
                dy = y - botY
                distanceSquared = dx * dx + dy * dy
            in
                distanceSquared <= maxRadius * maxRadius

        -- Objects in arena
        objectsInRange = 
            w.arena.objects
                |> List.filter(\obj ->
                    case obj of
                        Wall (x, y) -> isInRadarRange (x, y)
                        HealPack (x, y) _ -> isInRadarRange (x, y)
                        BotObj _ _ -> False
                )
                |> List.map(\obj -> 
                    case obj of 
                        Wall (x, y) -> ((x, y), obj)
                        HealPack (x, y) _ -> ((x, y), obj)
                        BotObj id pos -> (pos, BotObj id pos)
                )
        -- Bots in arena
        botsInRange =
            w.bots
                |> List.filter (\enemyBot ->
                    enemyBot.id /= b.id &&
                    enemyBot.alive &&
                    isInRadarRange enemyBot.pos
                )
                |> List.map(\bot -> (bot.pos, BotObj bot.id bot.pos))
    in
        objectsInRange ++ botsInRange

runBot : World -> Bot -> (Bot, Maybe Model.BotId)
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
                        _ -> False) w.arena.objects
                    ) &&
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
        ({ b | pc = b.pc + 1, pos = newPos }, Nothing)

    -- Turn 0, 90, 180, 270 degrees (up, right, down, left)
    (Turn n :: _) -> ({ b | pc = b.pc + 1, dirDeg = modBy 360 (b.dirDeg + n) }, Nothing)
    -- Scan environment, view angle is 90 degrees -> 45 degrees left and right
    (Scan :: _)   -> ({ b | pc = b.pc + 1, viewEnv = scanEnvironment w b }, Nothing)    
    -- Fire at coordinate
    (Fire x y :: _) -> 
        let
            -- Find if there is a bot at the target
            hitBotId = 
                w.bots
                    |> List.filter (\bot -> bot.pos == (x, y) && bot.alive && bot.id /= b.id)
                    |> List.head
                    |> Maybe.map .id
        in 
        ({ b | pc = b.pc + 1 }, hitBotId)
        
    _ -> ({ b | pc = b.pc + 1 }, Nothing) -- No instruction or end of program

run : World -> (List Bot, List Model.BotId)
run w = 
    w.bots
        |> List.map(\b -> runBot w b)
        |> List.foldr
            (\(updatedBot, maybeHitId) (bots, hitIds) ->
                (updatedBot :: bots
                , case maybeHitId of
                    Just hitId -> hitId :: hitIds
                    Nothing -> hitIds
                )
            )
            ([], [])


tick : World -> World
tick world = 
    let
        (updatedBots, hitIds) = run world
        -- Mark hit bots as dead
        finalBots =
            updatedBots
                |> List.map (\b ->
                    if List.member b.id hitIds then
                        { b | alive = False, hp = 0 } -- Bot is hit and dies
                    else
                        b
                )
    in
    { world
    | tick = world.tick + 1
    , bots = finalBots
    }