module Engine exposing (tick)

import Model exposing (World, Instr(..), Obj(..))
import Model exposing (Cond(..))
import Model exposing (BotEntity)

getObjPos : Obj -> Model.Coord
getObjPos obj =
    case obj of
        Wall coord       -> coord
        HealPack coord _ -> coord
        Bot entity       ->  entity.pos

isInRadarRange : Model.Coord -> Model.Coord -> Int -> Bool
isInRadarRange (botX, botY) (x, y) n = 
    let
        dx = x - botX
        dy = y - botY
        distanceSquared = dx * dx + dy * dy
    in
        distanceSquared <= n * n


scanEnvironment : World -> Obj -> List (Model.Coord, Obj)
scanEnvironment w obj =
    -- Scans the environment like a radar with max range (hard coded at the moment)
    let
        maxRadius = 4

        -- Bot position
        (botX, botY) = getObjPos obj

        -- Scans for Objects and Bots in arena, and pairs them with their coordinates
        objectsInRange =
            w.arena.objects
                |> List.filter(\o ->
                    case o of
                        Wall coord  -> isInRadarRange (botX, botY) coord maxRadius
                        HealPack coord _ -> isInRadarRange (botX, botY) coord maxRadius
                        Bot entity -> isInRadarRange (botX, botY) entity.pos maxRadius
                )
                |> List.map (\o -> (getObjPos o, o))
    in
        objectsInRange

runBot : World -> BotEntity -> BotEntity
runBot w b = case (List.drop b.pc b.program ) of
    -- Move as long as there is no wall or object in the way
    (Move n :: _) -> 
        let  
            -- Try to move step by step
            moveStepByStep steps (x, y) = 
                let
                    -- Get step direction based on bot orientation
                    (dx, dy) = case b.dirDeg of
                        0   -> (-1, 0)  -- Move up
                        90  -> (0, 1)   -- Move right
                        180 -> (1, 0)   -- Move down
                        270 -> (0, -1)  -- Move left
                        _   -> (0, 0)   -- Don't move 

                    -- Check if position is valid (within bounds, no wall, no bot)
                    isValidPos (x2, y2) = 
                        x2 >= 0 && x2 < Tuple.second(w.arena.size) && y2 >= 0 && y2 < Tuple.first(w.arena.size) &&
                        not (List.any (\obj ->
                            case obj of
                                Wall coord -> coord == (x2, y2)
                                _ -> False) w.arena.objects
                            ) &&
                        not (List.any (\bot -> bot.id /= b.id && bot.alive && bot.pos == (x2, y2)) w.bots)
                in
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
    (Scan :: _)   -> { b | pc = b.pc + 1, viewEnv = scanEnvironment w (Bot b) }
    -- Fire at coordinate
    (Fire _ _ :: _) ->  { b | pc = b.pc + 1 }
    -- No instruction or end of program
    _ -> { b | pc = b.pc + 1 } 

run : World -> (List BotEntity, List Model.BotId)
run w =
    w.bots
        |> List.map (\b ->
            let
                updatedBot = runBot w b
                -- If the bot just fired, try to find a hit
                maybeHitId =
                    case List.drop b.pc b.program of
                        (Fire x y :: _) ->
                            w.bots
                                |> List.filter (\bot -> bot.pos == (x, y) && bot.alive && bot.id /= b.id)
                                |> List.head
                                |> Maybe.map .id
                        _ -> Nothing
            in
            (updatedBot, maybeHitId)
        )
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
                |> List.map (\bot ->
                    if List.member bot.id hitIds then
                        { bot | alive = False, hp = 0 } -- Bot is hit and dies
                    else
                        bot
                )
    in
    { world
    | tick = world.tick + 1
    , bots = finalBots
    }