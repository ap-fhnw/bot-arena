module ParseErrorTest exposing (tests)

import Expect
import Parse exposing (parseScript)
import Test exposing (..)

-- Complete Script Parser
run : String -> Result String ( List (), String )
run s = parseScript (String.toUpper s)
    |> Result.mapError Tuple.second
    |> Result.map (\( instrs, rest ) -> ( List.map (\_ -> ()) instrs, rest ))

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
    ]