module Parse exposing (parseBotScript, parseBotScriptSave)

import Model exposing (Instr(..), Cond(..))

type alias Parser a = String -> Result (Int, String) ( a, String )

-- Similiar to pure from Haskell Applicative
succeed : a -> Parser a
succeed a = \input -> Ok (a, input)

-- Similiar to empty from Haskell Alternative
fail : String -> Parser a
fail msg = \_ -> Err ( 0, msg )

-- Get consumed parser length
consumed : String -> String -> Int
consumed before after = String.length before - String.length after

-- String Parser helper functions
satDesc : (Char -> Bool) -> String -> Parser Char
satDesc pred desc = \input -> case String.uncons input of -- Split a non-empty string into its head and tail
    Just (c, rest) ->
        if pred c then 
            Ok (c, rest)
        else
            Err (0, "Expected " ++ desc ++ ", but found '" ++ String.fromChar c ++ "'")
    Nothing ->
        Err (0, "Expected " ++ desc ++ ", but got end of input")

sat : (Char -> Bool) -> Parser Char
sat pred = satDesc pred "character matching predicate"

takeWhile : (Char -> Bool) -> Parser String
takeWhile pred =
    let
        step acc = oneOf
            [ sat pred |> andThen (\c -> step (c :: acc))
            , succeed (String.fromList (List.reverse acc))
            ]
    in
        step []

char : Char -> Parser Char
char ch = satDesc (\c -> c == ch) ("character '" ++ String.fromChar ch ++ "'")

isSpace : Char -> Bool
isSpace c = c == ' ' || c == '\n'

spaces : Parser ()
spaces = takeWhile isSpace |> map (\_ -> ())

signedDigits : Bool -> Parser Int
signedDigits isNeg = takeWhile Char.isDigit |> andThen
    (\s -> 
        case String.toInt (if isNeg then "-" ++ s else s) of
            Just n -> succeed n
            Nothing -> fail "invalid integer literal"
    )

intToken : Parser Int
intToken = 
    let 
        number = oneOf
            [ char '-' |> andThen (\_ -> signedDigits True)
            , signedDigits False
            ]
    in
        ignoreRight number spaces

token : String -> Parser String
token str = ignoreLeft spaces (ignoreRight (string str) spaces)

string : String -> Parser String
string target =
    let
        step : List Char -> Parser String
        step chars = case chars of
            [] -> succeed target
            c :: cs -> char c |> andThen (\_ -> step cs)
    in
        step (String.toList target)

map : (a -> b) -> Parser a -> Parser b
map f p = \input -> case p input of
    Ok (a, rest) -> Ok (f a, rest)
    Err e -> Err e

apply : Parser (a -> b) -> Parser a -> Parser b
apply pf pa = \input -> case pf input of
    Ok (f, rest1) -> case pa rest1 of
        Ok (a, rest2) -> Ok (f a, rest2)
        Err (c2, msg) -> Err ( consumed input rest1 + c2, msg )
    Err e -> Err e

-- Sequences
map2 : (a -> b -> c) -> Parser a -> Parser b -> Parser c
map2 f pa pb = apply (map f pa) pb

ignoreLeft : Parser a -> Parser b -> Parser b
ignoreLeft p1 p2 = map2 (\_ b -> b) p1 p2

ignoreRight : Parser a -> Parser b -> Parser a
ignoreRight p1 p2 = map2 (\a _ -> a) p1 p2

oneOf : List ( Parser a ) -> Parser a
oneOf parsers = \input ->
    let
        try ps lastErr = case ps of
            [] -> Err lastErr
            p :: rest -> case p input of
                Ok res -> Ok res
                Err (cons, msg) ->
                    if cons == 0 then -- nothing consumed
                        let
                            -- syntax error ignore
                            relevant = not (String.startsWith "Expected character" msg)
                        in
                            try rest (if relevant then (cons, msg) else lastErr)
                    else -- something consumed
                        Err (cons, msg)
    in
        try parsers (0, "No alternatives matched")


some : Parser a -> Parser (List a)
some p = map2 (::) p (many p)

many : Parser a -> Parser (List a)
many p = oneOf
    [ map2 (::) p (lazy (\_ -> many p))
    , succeed []
    ]

andThen : (a -> Parser b) -> Parser a -> Parser b
andThen f p = \input -> case p input of
    Ok (a, rest) -> case f a rest of
            Ok res -> Ok res
            Err (c2, msg) -> Err (consumed input rest + c2, msg)
    Err e -> Err e

endOfInput : Parser ()
endOfInput = \input ->
    if String.isEmpty input then
        Ok ((), "")
    else
        Err (0, "Expected end of input")

lazy : (() -> Parser a) -> Parser a
lazy thunk = \input -> thunk () input

lazyInstr : (() -> Parser a) -> Parser a
lazyInstr thunk =
    ignoreLeft spaces (lazy thunk) -- removes spaces before

-- Parser
turnArg : Parser Int
turnArg = oneOf
    [ map (\_ -> -90)  (token "LEFT")
    , map (\_ ->  90)  (token "RIGHT")
    , map (\_ -> 180)  (token "BEHIND")
    , intToken
    ]

parseCond : Parser Cond
parseCond = oneOf
        [ map Not (ignoreLeft (token "NOT") (lazy (\_ -> parseCond)))
        , map (\_ -> EnemyAhead) (token "ENEMYAHEAD")
        , map (\_ -> WallAhead)  (token "WALLAHEAD")
        , map (\_ -> LowHp)      (token "LOWHP")
        , map (\_ -> Always)     (token "TRUE")
        ]

parseTurnDir : Parser Model.TurnDir
parseTurnDir = oneOf
    [ map (\_ -> Model.RIGHT) (token "RIGHT")
    , map (\_ -> Model.LEFT)  (token "LEFT")
    , map (\_ -> Model.AROUND)  (token "AROUND")
    , map (\_ -> Model.STRAIGHT)  (token "STRAIGHT")
    ]

parseInstr : Parser Instr
parseInstr = oneOf
        -- IF cond THEN instr ELSE instr
        [ ignoreLeft (token "IF") parseCond
            |> andThen (\cond -> ignoreLeft (token "THEN") (lazyInstr (\_ -> parseInstr))
                    |> andThen (\thenInstr -> ignoreLeft (token "ELSE") (lazyInstr (\_ -> parseInstr))
                            |> map (\elseInstr -> IfThenElse cond thenInstr elseInstr
                            )
                    )
            )
        , map2 While 
            (ignoreLeft (token "WHILE") parseCond)
            (ignoreLeft (token "DO") (lazyInstr (\_ -> parseInstr)))
        , map2 Repeat
            (ignoreLeft (token "REPEAT") intToken)
            (lazyInstr (\_ -> parseInstr))
        , map2 Fire
            (ignoreLeft (token "FIRE") intToken)
            intToken

        , map Move (ignoreLeft (token "MOVE") intToken)
        , map Turn (ignoreLeft (token "TURN") parseTurnDir)
        , ignoreLeft (token "SCAN") (succeed Scan)
        , ignoreLeft (token "NOTHING") (succeed NoOp)
        ]

-- Parse complete string
parseScript : Parser (List Instr)
parseScript = map2 (\instrs _ -> instrs) (some (ignoreLeft spaces parseInstr)) (ignoreLeft spaces endOfInput) 

-- Top level Parser
parseBotScript : String -> Result String (List Instr)
parseBotScript input =
    parseScript (String.toUpper input)
        |> Result.map Tuple.first
        |> Result.mapError Tuple.second

parseBotScriptSave : String -> List Instr
parseBotScriptSave = parseBotScript >> Result.withDefault []