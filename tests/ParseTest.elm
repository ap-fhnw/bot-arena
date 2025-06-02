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

        ,describe "TURN LEFT/RIGHT/AROUND/STRAIGHT"
            [ test "LEFT" <|
                \_ -> parse "TURN LEFT"     |> Expect.equal (Just <| Turn LEFT)
            , test "RIGHT" <|
                \_ -> parse "TURN RIGHT"    |> Expect.equal (Just <| Turn RIGHT)
            , test "AROUND" <|
                \_ -> parse "TURN AROUND"   |> Expect.equal (Just <| Turn AROUND)
            , test "STRAIGHT" <|
                \_ -> parse "TURN STRAIGHT" |> Expect.equal (Just <| Turn STRAIGHT)
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
                parse """
                REPEAT 10
                    IF WALLAHEAD
                        THEN TURN RIGHT
                    ELSE MOVE 1
                """
                |> Expect.equal
                    ( Just <| Repeat 10 (IfThenElse WallAhead (Turn RIGHT) (Move 1)) )
        ]
        , describe "Block-Parsing [ … ]"
        [ test "MOVE + TURN Block" <|
            \_ ->
                parse "[ MOVE 1 TURN LEFT ]"
                    |> Expect.equal
                        (Just <| Seq [ Move 1, Turn LEFT ])
        , test "IF Block" <|
            \_ ->
                parse "IF ENEMYAHEAD THEN [ FIRE TURN RIGHT ] ELSE [ MOVE 2 ]"
                    |> Expect.equal
                        (Just <| IfThenElse EnemyAhead
                            (Seq [ Fire, Turn RIGHT ])
                            (Seq [ Move 2 ])
                        )
        , test "REPEAT Block" <|
            \_ ->
                parse """
                REPEAT 3 [ MOVE 1 [ TURN LEFT TURN RIGHT ] ]
                """
                    |> Expect.equal
                        (Just <| Repeat 3
                            (Seq
                                [ Move 1
                                , Seq [ Turn LEFT, Turn RIGHT ]
                                ]
                            )
                        )

        , test "WHILE Block" <|
            \_ ->
                parse "WHILE TRUE DO [ SCAN MOVE 1 ]"
                    |> Expect.equal
                        (Just <| While Always (Seq [ Scan, Move 1 ]))

        , test "empty Block []" <|
            \_ ->
                parse "[]"
                    |> Expect.equal
                        (Just <| NoOp)
        ]
    ]
