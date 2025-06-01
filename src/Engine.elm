module Engine exposing (tick)

import Model exposing (World, Instr(..), Obj(..))
import Model exposing (Cond(..))
import Model exposing (BotEntity)
import Model exposing (TurnDir(..))

getObjPos : Obj -> Model.Coord
getObjPos obj =
    case obj of
        Wall coord       -> coord
        HealPack coord _ -> coord
        Bot entity       ->  entity.pos

-- Helper functions 
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

isInBounds : World -> Model.Coord -> Bool
isInBounds w c =
    let
        (x, y) = c
    in
    x >= 0 && x < Tuple.second(w.arena.size) && y >= 0 && y < Tuple.first(w.arena.size)

liveBotAt : BotEntity -> Model.Coord -> Bool
liveBotAt bot coord =
    bot.alive && bot.pos == coord

-- Not really used now, but could be useful later - with other weapons or other enhancements
scanEnvironment : World -> BotEntity -> List (Model.Coord, Obj)
scanEnvironment w bot =
    -- Scans the environment like a radar with max range (hard coded at the moment)
    let
        maxRadius = 4

        -- Scans for Objects and Bots in arena, and pairs them with their coordinates
        objectsInRange =
            List.filter(\o -> isInRadarRange bot [o] maxRadius) w.arena.objects
                |> List.map (\o -> (getObjPos o, o))
        -- Scans for Bots in arena, and pairs them with their coordinates (right now scans himself too)
        botsInRange =
            List.filter (\b -> isInRadarRange bot [Bot b] maxRadius) w.bots
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
                isValidPos (x2, y2) =  isInBounds w (x2, y2) &&
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

turnBot : BotEntity -> TurnDir -> Int
turnBot b n =
    if not b.alive then
        -- If the bot is not alive, it cannot turn
        b.dirDeg
    else
    case n of
        STRAIGHT -> b.dirDeg
        RIGHT    -> modBy 360 (b.dirDeg + 90)
        LEFT     -> modBy 360 (b.dirDeg - 90)
        AROUND   -> modBy 360 (b.dirDeg + 180)

fire : World -> BotEntity -> Int -> List Model.Coord
fire w b n =
    if not b.alive then
        [] -- Can't Shoot at anything, I'm dead
    else
    let
        ((dx, dy), (x, y)) = getBotDirAndPos b
        bulletPath : Int -> Int -> Int -> List Model.Coord -> List Model.Coord
        bulletPath currentDist currentX currentY acc = 
            if currentDist > n then
                acc
            else
                let
                    currentPos = (currentX, currentY)

                    -- Check if current position is a WALL -> yes then stop
                    hasWall = List.any(\obj -> 
                        case obj of
                            Wall coord -> coord == currentPos
                            _-> False
                        ) w.arena.objects
                    
                    -- Check if current position is still within the world bounds
                    inBounds = isInBounds w (currentX, currentY)
                in
                if not inBounds || hasWall then
                    acc
                else
                    bulletPath
                        (currentDist + 1)
                        (currentX + dx)
                        (currentY + dy)
                        (currentPos :: acc)
                        
    in
    bulletPath 1 (x + dx) (y + dy) [] |> List.reverse

evalCond : World -> BotEntity -> Cond -> Bool
evalCond w b cond =
    case cond of
        EnemyAhead ->
            -- Check if there is an enemy bot in front of the bot
            let
                viewLength = 4
                ((dx, dy), (x, y)) = getBotDirAndPos b
                checkPositions = 
                    List.range 1 viewLength
                        |> List.map (\dist -> (x + dist * dx, y + dist * dy))
                
                -- Check for walls
                wallAt pos = List.any (\obj -> 
                    case obj of
                        Wall coord -> coord == pos
                        _ -> False) w.arena.objects

                -- Check for enemy, wall or out of bounds
                checkLine positions =
                    case positions of
                        [] -> False
                        pos :: rest ->
                            if not (isInBounds w pos) || (wallAt pos) then
                                False
                            else
                                let
                                    enemyAtPos = List.any (\bot -> liveBotAt bot pos && bot.id /= b.id) w.bots
                                in
                                if enemyAtPos then
                                    True
                                else
                                    checkLine rest
            in
            checkLine checkPositions

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

        Always ->
            True

        Not c ->
            not (evalCond w b c)

executeInstr : World -> BotEntity -> Instr -> BotEntity
executeInstr w b instr = case instr of
    -- Move as long as there is no wall or object in the way
    Move n -> { b | pc = b.pc + 1, pos = moveBot w b n}
    -- Turn RIGHT, LEFT or AROUND
    Turn n  -> { b | pc = b.pc + 1, dirDeg = turnBot b n }
    -- Scan environment, radarlike with radius
    Scan   -> { b | pc = b.pc + 1, viewEnv = scanEnvironment w b }
    -- Fire at coordinate --> see run world function
    Fire ->  { b | pc = b.pc + 1, fireAt = fire w b b.range }
    -- If-then-else instruction
    IfThenElse cond ifTrue ifFalse ->
        if evalCond w b cond then
            insertInstrAtNextPc ifTrue b
        else
            -- If condition is false, execute the false branch
            insertInstrAtNextPc ifFalse b

    -- While loop
    While cond body ->
        if evalCond w b cond then
            -- If condition is true, execute the body
            let
                botAfterBody = case body of
                    Move n -> { b | pos = moveBot w b n }
                    Turn n -> { b | dirDeg = turnBot b n }
                    Scan -> { b | viewEnv = scanEnvironment w b }
                    Fire -> { b | pc = b.pc + 1 } -- Fire does not change the bot state
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
    Repeat n body ->
        if n > 0 then
            let
                -- Execute the first body instruction immediately
                botAfterFirstExecution = executeInstr w b body

                -- Create a list with remaining n-1 repetitions
                remainingRepeats =
                    if n > 1 then
                        List.repeat (n - 1) body
                    else
                        []

                -- Construct the new program with remaining repetitions
                newProgram =
                    if List.isEmpty remainingRepeats then
                        -- No remaining repeats, just continue with the original program
                        botAfterFirstExecution.program
                    else
                        -- Insert remaining repeats after the current position
                        List.take (b.pc + 1) b.program ++
                        remainingRepeats ++
                        List.drop (b.pc + 1) b.program
            in
            -- Return the bot after executing the first instruction
            { botAfterFirstExecution | program = newProgram }
        else
            -- Skip the repeat if n is 0
            { b | pc = b.pc + 1 }

    -- No instruction or end of program
    _ -> { b | pc = b.pc + 1 }

-- Used in If-Then-Else Conditional
insertInstrAtNextPc : Instr -> BotEntity -> BotEntity
insertInstrAtNextPc instr bot =
    let
        newProgram = List.take (bot.pc + 1) bot.program ++ (instr :: List.drop (bot.pc + 1) bot.program)
    in
    { bot | pc = bot.pc + 1, program = newProgram }

runBot : World -> BotEntity -> BotEntity
runBot w b = 
    let
        currentInstr = List.drop b.pc b.program |> List.head |> Maybe.withDefault NoOp
    in
    executeInstr w b currentInstr

run : World -> (List BotEntity, List Model.BotId)
run w =
    w.bots
        |> List.map (\b ->
            let
                updatedBot = runBot w b
                -- If the bot just fired, try to find a hit
                hitIds = List.concatMap(\pos ->
                                w.bots
                                    |> List.filter(\target ->
                                        target.id /= updatedBot.id &&
                                        liveBotAt target pos
                                        )
                                        |> List.map .id
                                    ) updatedBot.fireAt
                        
                -- Clear the fireAt field after firing
                finalBot = { updatedBot | fireAt = []}
            in
            (finalBot, hitIds)
        )
        |> List.foldr
            (\(updatedBot, hitIds) (bots, allHitIds) ->
                (updatedBot :: bots, hitIds ++ allHitIds)
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