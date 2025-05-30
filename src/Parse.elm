module Parse exposing (parseBotScript)

import Model exposing (Instr(..), Cond(..))

type alias Parser a = String -> Maybe (a, String)

-- Similiar to pure from Haskell Applicative
succeed : a -> Parser a
succeed a = \input -> Just (a, input)

-- Similiar to empty from Haskell Alternative
fail : Parser a
fail = \_ -> Nothing

-- String Parser helper functions
sat : (Char -> Bool) -> Parser Char
sat pred = \input -> case String.uncons input of -- Split a non-empty string into its head and tail
    Just (c, rest) ->
        if pred c then
            Just (c, rest)
        else
            Nothing
    Nothing -> Nothing

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
char ch = sat (\c -> c == ch)

isSpace : Char -> Bool
isSpace c = c == ' ' || c == '\n'

spaces : Parser ()
spaces = takeWhile isSpace |> map (\_ -> ())

signedDigits : Bool -> Parser Int
signedDigits isNeg = takeWhile Char.isDigit |> andThen
    (\s -> 
        case String.toInt (if isNeg then "-" ++ s else s) of
            Just n -> succeed n
            Nothing -> fail
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
        step : List Char -> Parser ()
        step chars = case chars of
            [] -> succeed ()
            c :: cs -> char c |> andThen (\_ -> step cs)
    in
        map (\_ -> target) (step (String.toList target))

map : (a -> b) -> Parser a -> Parser b
map f p = \input -> case p input of
    Just (a, rest) -> Just (f a, rest)
    Nothing -> Nothing

apply : Parser (a -> b) -> Parser a -> Parser b
apply pf pa = \input -> case pf input of
    Just (f, rest1) -> case pa rest1 of
        Just (a, rest2) -> Just (f a, rest2)
        Nothing -> Nothing
    Nothing -> Nothing

-- Sequences
map2 : (a -> b -> c) -> Parser a -> Parser b -> Parser c
map2 f pa pb =
    apply (map f pa) pb

ignoreLeft : Parser a -> Parser b -> Parser b
ignoreLeft p1 p2 = map2 (\_ b -> b) p1 p2

ignoreRight : Parser a -> Parser b -> Parser a
ignoreRight p1 p2 = map2 (\a _ -> a) p1 p2

oneOf : List (Parser a) -> Parser a
oneOf parsers = \input -> case parsers of 
    p :: ps -> case p input of
        Just result -> Just result
        Nothing -> (oneOf ps) input

    [] -> Nothing

many : Parser a -> Parser (List a)
many p = oneOf
    [ map2 (::) p (lazy (\_ -> many p))
    , succeed []
    ]

andThen : (a -> Parser b) -> Parser a -> Parser b
andThen f p = \input -> case p input of
    Just (a, rest) -> f a rest
    Nothing -> Nothing

endOfInput : Parser ()
endOfInput = \input ->
    if String.isEmpty input then
        Just ((), "")
    else
        Nothing

lazy : (() -> Parser a) -> Parser a
lazy thunk = \input -> thunk () input

lazyInstr : (() -> Parser a) -> Parser a
lazyInstr thunk =
    ignoreLeft spaces (lazy thunk) -- removes spaces before

-- Parser
parseCond : Parser Cond
parseCond = oneOf
        [ map Not (ignoreLeft (token "NOT") (lazy (\_ -> parseCond)))
        , map (\_ -> EnemyAhead) (token "ENEMYAHEAD")
        , map (\_ -> WallAhead)  (token "WALLAHEAD")
        , map (\_ -> LowHp)      (token "LOWHP")
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
        , map Turn (ignoreLeft (token "TURN") intToken)
        , ignoreLeft (token "SCAN") (succeed Scan)
        , ignoreLeft (token "NOTHING") (succeed NoOp)
        ]

-- Parse complete string
parseScript : Parser (List Instr)
parseScript = map2 (\instrs _ -> instrs) (many (parseInstr)) (ignoreLeft spaces endOfInput) 

-- Top level Parser
parseBotScript : String -> List Instr
parseBotScript input =
    case parseScript (String.toUpper input) of
        Just (instrs, "") -> instrs
        _ -> []
