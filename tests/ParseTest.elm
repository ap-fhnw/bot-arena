module ParseTest exposing (tests)

import Expect
import Parse exposing (parseBotScript)
import Test exposing (..)
import Model exposing (Instr(..), Cond(..), TurnDir(..))

-- Helper function
parse : String -> Maybe Instr
parse line = case parseBotScript line of
    Ok [ i ] -> Just i
    _        -> Nothing


-- The Parser test
tests : Test
tests = describe "parseBotScript"
    [ describe "Einzel-Instruktionen"
        [ test "MOVE" <|
            \_ ->
                parse "MOVE 3"
                    |> Expect.equal (Just <| Move 3)

        , test "TURN" <|
            \_ ->
                parse "TURN LEFT"
                    |> Expect.equal (Just <| Turn LEFT)

        ,describe "TURN LEFT/RIGHT/BEHIND"
            [ test "LEFT" <|
                \_ -> parse "TURN LEFT"   |> Expect.equal (Just <| Turn -90)

            , test "RIGHT" <|
                \_ -> parse "TURN RIGHT"  |> Expect.equal (Just <| Turn 90)

            , test "BEHIND" <|
                \_ -> parse "TURN BEHIND" |> Expect.equal (Just <| Turn 180)
            ]

        , test "SCAN" <|
            \_ ->
                parse "SCAN"
                    |> Expect.equal (Just Scan)

        , test "FIRE" <|
            \_ ->
                parse "FIRE"
                    |> Expect.equal (Just <| Fire)

        , test "NOOP / NOTHING" <|
            \_ ->
                parse "NOTHING"
                    |> Expect.equal (Just NoOp)

        , test "REPEAT … Instr" <|
            \_ ->
                parse "REPEAT 4 MOVE 1"
                    |> Expect.equal (Just <| Repeat 4 (Move 1))

        , test "IF … THEN … ELSE …" <|
            \_ ->
                parse "IF ENEMYAHEAD THEN FIRE ELSE MOVE 2"
                    |> Expect.equal
                        (Just <|
                            IfThenElse EnemyAhead
                                (Fire)
                                (Move 2)
                        )

        , test "WHILE … DO …" <|
            \_ ->
                parse "WHILE LOWHP DO MOVE 1"
                    |> Expect.equal
                        (Just <| While LowHp (Move 1))

        , test "WHILE TRUE DO …" <|
            \_ ->
                parse "WHILE TRUE DO MOVE 1"
                    |> Expect.equal (Just <| While Always (Move 1))
        ]
    , describe "Conditions (inkl. NOT …)"
        [ test "ENEMYAHEAD" <|
            \_ ->
                parse "IF ENEMYAHEAD THEN MOVE 1 ELSE MOVE 2"
                    |> Expect.equal
                        (Just <| IfThenElse EnemyAhead (Move 1) (Move 2))

        , test "NOT WALLAHEAD" <|
            \_ ->
                parse "IF NOT WALLAHEAD THEN TURN RIGHT ELSE MOVE 1"
                    |> Expect.equal
                        (Just <|
                            IfThenElse (Not WallAhead)
                                (Turn RIGHT)
                                (Move 1)
                        )
        , test "multiline REPEAT + IF Block" <|
            \_ ->
                case parseBotScript """
                REPEAT 10
                    IF WALLAHEAD
                        THEN TURN RIGHT
                    ELSE MOVE 1
                """
                |> Expect.equal
                    [ Repeat 10 (IfThenElse WallAhead (Turn RIGHT) (Move 1)) ]

                ]
    ]
