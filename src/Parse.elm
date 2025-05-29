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
        ["MOVE", nStr] ->
            parseInt nStr |> Maybe.map Move

        ["TURN", nStr] ->
            parseInt nStr |> Maybe.map Turn

        ["SCAN"] ->
            Just Scan

        ["NOTHING"] ->
            Just NoOp

        ["FIRE", xStr, yStr] ->
            case (parseInt xStr, parseInt yStr) of
                (Just x, Just y) -> Just (Fire x y)
                _ -> Nothing

        "REPEAT" :: nStr :: rest ->
            case (parseInt nStr, parseInstrFromWords rest) of
                (Just n, Just instr) ->
                    Just (Repeat n instr)

                _ ->
                    Nothing

        -- IF cond THEN instr ELSE instr
        "IF" :: rest ->
            case parseCond rest of
                Just (cond, "THEN" :: thenElseRest) ->
                    case splitAt "ELSE" thenElseRest of
                        Just (thenPart, elsePart) ->
                            case (parseInstrFromWords thenPart, parseInstrFromWords elsePart) of
                                (Just th, Just el) ->
                                    Just (IfThenElse cond th el)
                                _ -> Nothing
                        Nothing -> Nothing

                _ -> Nothing

        -- WHILE cond DO instr
        "WHILE" :: rest ->
            case parseCond rest of
                Just (cond, "DO" :: afterDo) ->
                    case parseInstrFromWords afterDo of
                        Just instr ->
                            Just (While cond instr)
                        Nothing -> Nothing

                _ -> Nothing

        _ ->
            Nothing

parseCond : List String -> Maybe (Cond, List String)
parseCond words =
    case words of
        "NOT" :: rest ->
            case parseCond rest of
                Just (c, remaining) ->
                    Just (Not c, remaining)
                Nothing ->
                    Nothing

        "ENEMYAHEAD" :: rest ->
            Just (EnemyAhead, rest)

        "WALLAHEAD" :: rest ->
            Just (WallAhead, rest)

        "LOWHP" :: rest ->
            Just (LowHp, rest)

        _ ->
            Nothing
