module Parse exposing (parseBotScript)

import Model exposing (Instr(..), Cond(..))

-- Help functions
parseInt : String -> Maybe Int
parseInt = String.toInt

strip : String -> String
strip str = str |> String.trim

splitWords : String -> List String
splitWords = String.words

normalizeWords : List String -> List String
normalizeWords = List.map String.toUpper

findIndex : (a -> Bool) -> List a -> Maybe Int
findIndex predicate list =
    let
        helper idx rest =
            case rest of
                [] -> Nothing
                x :: xs ->
                    if predicate x then Just idx else helper (idx + 1) xs
    in
    helper 0 list

splitAt : String -> List String -> Maybe (List String, List String)
splitAt marker list =
    case findIndex ((==) marker) list of
        Just i ->
            Just (List.take i list, List.drop (i + 1) list)

        Nothing ->
            Nothing

-- Parser 
parseBotScript : String -> List Instr
parseBotScript input =
    input
        |> String.lines                             -- Split into lines
        |> List.filter (\l -> String.trim l /= "")  -- Remove empty lines
        |> List.filterMap parseLine                 -- Try to parse each line

parseLine : String -> Maybe Instr
parseLine line =
    line
        |> strip
        |> splitWords
        |> normalizeWords -- case-insensitive parsing
        |> parseInstrFromWords

parseInstrFromWords : List String -> Maybe Instr
parseInstrFromWords words =
    case words of
        ["move", nStr] ->
            parseInt nStr |> Maybe.map Move

        ["turn", nStr] ->
            parseInt nStr |> Maybe.map Turn

        ["scan"] ->
            Just Scan

        ["nothing"] ->
            Just NoOp

        ["fire", xStr, yStr] ->
            case (parseInt xStr, parseInt yStr) of
                (Just x, Just y) -> Just (Fire x y)
                _ -> Nothing

        "repeat" :: nStr :: rest ->
            case (parseInt nStr, parseInstrFromWords rest) of
                (Just n, Just instr) ->
                    Just (Repeat n instr)

                _ ->
                    Nothing

        "while" :: condStr :: "do" :: rest ->
            case (parseCond condStr, parseInstrFromWords rest) of
                (Just cond, Just instr) ->
                    Just (While cond instr)
                _ -> Nothing

        -- IF cond THEN instr ELSE instr
        "if" :: condStr :: "then" :: thenElseRest ->
            case splitAt "else" thenElseRest of
                Just (thenPart, elsePart) ->
                    case (parseCond condStr, parseInstrFromWords thenPart, parseInstrFromWords elsePart) of
                        (Just cond, Just th, Just el) ->
                            Just (IfThenElse cond th el)
                        _ -> Nothing
                Nothing ->
                    Nothing

        "ENEMYAHEAD" :: rest ->
            Just (EnemyAhead, rest)

        "LOWHP" :: rest ->
            Just (LowHp, rest)

        _ ->
            Nothing

parseCond : String -> Maybe Cond
parseCond str =
    case String.toUpper str of
        "ENEMYAHEAD" -> Just EnemyAhead
        "LOWHP" -> Just LowHp
        _ -> Nothing
