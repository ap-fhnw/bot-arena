module ParseErrorTest exposing (tests)

import Expect
import Parse exposing (parseBotScript)
import Test exposing (..)

-- Complete Script Parser
run : String -> Result String ( List () )
run s = parseBotScript (String.toUpper s)
    |> Result.map (List.map (\_ -> ()))

tests : Test
tests = describe "Fehlermeldungen des Parsers"
    [ test "Ungültiges Integer-Literal" <|
        \_ ->
            run "MOVE X"
                |> Expect.equal (Err "invalid integer literal")

    , test "Unerwartetes Zeichen innerhalb eines Schlüsselworts" <|
        \_ ->
            run "MOVX 1"
                |> Expect.equal (Err "Expected character 'E', but found 'X'")

    , test "Eingabe nicht komplett konsumiert" <|
        \_ ->
            run "MOVE 1 EXTRA"
                |> Expect.equal (Err "Expected end of input")

    , test "Keine Alternative greift" <|
        \_ ->
            run "BLABLA"
                |> Expect.equal (Err "No alternatives matched")
    , test "Ende vom Block fehlt" <|
            \_ ->
                run "["
                    |> Expect.equal (Err "Expected character ']', but got end of input")
    , test "Anfang vom Block fehlt" <|
            \_ ->
                run "]"
                    |> Expect.equal (Err "No alternatives matched")
    ]