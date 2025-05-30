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

isInRadarRange : BotEntity -> List (Obj) -> Int -> Bool
isInRadarRange bot objs n = 
    let
        (botX, botY) = bot.pos
    in
    List.any (\obj ->
        let
            (x, y) = getObjPos obj
            dx = x - botX
            dy = y - botY
            distanceSquared = dx * dx + dy * dy
        in
            distanceSquared <= n * n
    ) objs

liveBotAt : BotEntity -> Model.Coord -> Bool
liveBotAt bot coord =
    bot.alive && bot.pos == coord

scanEnvironment : World -> BotEntity -> List (Model.Coord, Obj)
scanEnvironment w obj =
    -- Scans the environment like a radar with max range (hard coded at the moment)
    let
        maxRadius = 4

        -- Scans for Objects and Bots in arena, and pairs them with their coordinates
        objectsInRange =
            List.filter(\o -> isInRadarRange obj [o] maxRadius) w.arena.objects
                |> List.map (\o -> (getObjPos o, o))
        -- Scans for Bots in arena, and pairs them with their coordinates (right now scans himself too)
        botsInRange =
            List.filter (\b -> isInRadarRange obj [Bot b] maxRadius) w.bots
                |> List.map (\b -> (b.pos, Bot b))
    in
        botsInRange ++ objectsInRange

getBotDirAndPos : BotEntity -> (Model.Coord, Model.Coord)
getBotDirAndPos bot =
    let
        dirVec = case bot.dirDeg of
            0   -> (-1, 0)  -- Up
            90  -> (0, 1)   -- Right
            180 -> (1, 0)   -- Down
            270 -> (0, -1)  -- Left
            _   -> (0, 0)   -- No movement
    in
    (dirVec, bot.pos)

-- Helper function to calculate movement with collision detection
moveBot : World -> BotEntity -> Int -> Model.Coord
moveBot w b steps =
    if not b.alive then
        -- If the bot is not alive, it cannot move
        b.pos
    else
    let
        -- Try to move step by step
        moveStepByStep remainingSteps (x, y) = 
            let
                -- Get step direction based on bot orientation
                ((dx, dy), (_, _)) = getBotDirAndPos b

                -- Check if position is valid (within bounds, no wall, no bot)
                isValidPos (x2, y2) = 
                    x2 >= 0 && x2 <= Tuple.second(w.arena.size) && 
                    y2 >= 0 && y2 <= Tuple.first(w.arena.size) &&
                    not (List.any (\obj ->
                        case obj of
                            Wall coord -> coord == (x2, y2)
                            _ -> False) w.arena.objects
                        ) &&
                    not (List.any (\bot -> 
                        bot.id /= b.id && liveBotAt bot (x2, y2)) w.bots)
            in
            if remainingSteps <= 0 then
                (x, y) -- reached target
            else
                let
                    nextPos = (x + dx, y + dy)
                in
                if isValidPos nextPos then
                    moveStepByStep (remainingSteps - 1) nextPos
                else
                    (x, y)
    in
    moveStepByStep steps b.pos

evalCond : World -> BotEntity -> Cond -> Bool
evalCond w b cond =
    case cond of
        EnemyAhead ->
            -- Check if there is an enemy bot in front of the bot
            let
                ((dx, dy), (x, y)) = getBotDirAndPos b
                targetPos = (x + dx, y + dy)
            in
            List.any (\bot -> liveBotAt bot targetPos && bot.id /= b.id) w.bots

        WallAhead ->
            let
                ((dx, dy), (x, y)) = getBotDirAndPos b
                targetPos = (x + dx, y + dy)
            in
            List.any (\obj -> 
                case obj of 
                    Wall coord -> coord == targetPos 
                    _ -> False
            ) w.arena.objects

        LowHp ->
            b.hp < (w.arena.maxHp // 2)

        Not c ->
            not (evalCond w b c)

-- Helper function to insert an instruction at the next program counter position
-- Used in If-Then-Else Conditional
insertInstrAtNextPc : Instr -> BotEntity -> BotEntity
insertInstrAtNextPc instr bot =
    let
        newProgram = List.take (bot.pc + 1) bot.program ++ (instr :: List.drop (bot.pc + 1) bot.program)
    in
    { bot | pc = bot.pc + 1, program = newProgram }

runBot : World -> BotEntity -> BotEntity
runBot w b = case (List.drop b.pc b.program ) of
    -- Move as long as there is no wall or object in the way
    (Move n :: _) -> { b | pc = b.pc + 1, pos = moveBot w b n}
    -- Turn 0, 90, 180, 270 degrees (up, right, down, left)
    (Turn n :: _) -> { b | pc = b.pc + 1, dirDeg = modBy 360 (b.dirDeg + n) }
    -- Scan environment, view angle is 90 degrees -> 45 degrees left and right
    (Scan :: _)   -> { b | pc = b.pc + 1, viewEnv = scanEnvironment w b }
    -- Fire at coordinate --> see run world function
    (Fire _ _ :: _) ->  { b | pc = b.pc + 1 }
    -- If-then-else instruction
    (IfThenElse cond ifTrue ifFalse :: _) ->
        if evalCond w b cond then
            insertInstrAtNextPc ifTrue b
        else
            -- If condition is false, execute the false branch
            insertInstrAtNextPc ifFalse b

    -- While loop
    (While cond body :: _) -> 
        if evalCond w b cond then
            -- If condition is true, execute the body
            let
                botAfterBody = case body of
                    Move n -> { b | pos = moveBot w b n }
                    Turn n -> { b | dirDeg = modBy 360 (b.dirDeg + n) }
                    Scan -> { b | viewEnv = scanEnvironment w b }
                    Fire _ _ -> { b | pc = b.pc + 1 } -- Fire does not change the bot state
                    _ -> { b | pc = b.pc + 1 } -- No operation, just move to next instruction

                -- Create a new Instruction list with the body repeated
                newProgram = List.take (b.pc) b.program ++ (While cond body :: List.drop (b.pc + 1) b.program)
            in
                -- Insert the repeated instructions at the current position
                { botAfterBody | program = newProgram }
            -- If condition is false, just skip to the next instruction
            else
                { b | pc = b.pc + 1 }

    -- Repeat instruction
    (Repeat n body :: _) ->
        if n > 0 then
            let
                -- Create a new Instruction list with the body repeated
                instrToRepeat = List.repeat n body
            in
            -- Update the program counter to point to the next instruction after the repeat
            if b.pc + 1 < List.length b.program then
                -- Insert the repeated instructions at the current position
                { b | pc = b.pc + 1, program = List.take (b.pc + 1) b.program ++ instrToRepeat ++ List.drop (b.pc + 1) b.program }
            else
                -- If we are at the end of the program, just append the repeated instructions 
                { b | pc = b.pc + 1, program = instrToRepeat ++ (List.drop (b.pc + 1) b.program) }
        else
            { b | pc = b.pc + 1 } -- Skip the repeat if n is 0


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
                                |> List.filter (\bot -> liveBotAt bot (x, y)) -- You can commit suicide
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