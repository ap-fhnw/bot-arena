module Parse exposing (parseBotScript)

import Model exposing (Instr(..))

parseBotScript : String -> List Instr
parseBotScript input = String.split "\n" input
    |> List.map (\l -> Move (String.length l))