module Parse exposing (parseBotScript)

import Model exposing (Instr(..))

-- Help functions
strip : String -> String
strip str = str |> String.trim

splitWords : String -> List String
splitWords = String.words

parseInt : String -> Maybe Int
parseInt = String.toInt

-- Parser 
parseBotScript : String -> List Instr
parseBotScript input = String.split "\n" (String.toLower input)
    |> List.map (\l -> case String.split " " l of
                    [ "move", n ] -> Move (String.toInt n |> Maybe.withDefault 0)        
                    [ "turn", n ] -> Turn (String.toInt n |> Maybe.withDefault 0)
                    [ "scan" ] -> Scan
                    [ "fire", x, y ] -> Fire (String.toInt x |> Maybe.withDefault 0) (String.toInt y |> Maybe.withDefault 0)
                    _ -> NoOp)