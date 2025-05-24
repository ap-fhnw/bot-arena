module Engine exposing (tick)

import Model exposing (World, Bot, Instr(..))

runBot : World -> Bot -> Bot
runBot w b = case (List.drop b.pc b.program) of
    (Move n :: _) -> { b | pc = b.pc + 1, pos = (n + Tuple.first b.pos, Tuple.second b.pos) }
    _ -> { b | pc = b.pc + 1 }

run : World -> List Bot
run w = List.map (\b -> runBot w b) w.bots

tick : World -> World
tick world = { world
    | tick = world.tick + 1
    , bots = run world
    }