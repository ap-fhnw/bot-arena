module View exposing (renderView, subscriptions)

import Debug exposing (toString)
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, spellcheck, value, type_, attribute)
import Html.Styled.Events exposing (onClick, onInput, onCheck)
import Model exposing (..)
import Styles exposing (..)
import Css.Global exposing (global, html, body)
import Json.Decode as D
import Browser.Events as Events
import Time
import Html.Styled.Attributes exposing (placeholder)

type Cell = B BotEntity | O Obj | Empty

listBots : List { a | name : String } -> String
listBots bots = (bots |> List.map (\b -> b.name ) |> String.join " vs. ")

renderView : Model -> Html.Html Msg
renderView model = (main_
    [ css
    [ grid
    , property "grid-template" "'editor map' auto / 1fr auto"
    , padding (Css.em 1)
    , height (pct 100)
    , boxSizing borderBox
    , fontFamilies [.value monospace]
    ]]
    [ global
        [ html [ property "color-scheme" "light dark" ]
        , body [ height (vh 100) ]
        ]
    , editor model [ property "grid-area" "editor"
        , height (pct 100)
        , overflow scroll ]
    , div
        [ css
        [ property "grid-area" "map"
        , grid
        , property "grid-template-rows" "auto 1fr auto"
        , height (pct 100)
        , overflow scroll
        ]]
        [   Html.Styled.table
            [ css
            [ borderCollapse collapse
            , margin auto
            , outsetBorder
            ]]
            ((List.range 0 (Tuple.second model.world.arena.size))
                |> List.map (renderRow model.world))
        , debugOutput
            [ css 
                [ displayFlex
                , flexDirection column
                , overflow scroll
                , lineHeight (num 1.5)
                ]
            ]
            [ setting []
                [ text "Auto-load"
                , input [ type_ "checkbox", onCheck AutoLoad, Html.Styled.Attributes.checked model.autoLoad ] []] 
            , setting []
                [ text "Auto-run"
                , input [ type_ "checkbox", onCheck AutoRun, Html.Styled.Attributes.checked model.autoRun ] []]
            , setting []
                [ text ("Tick " ++ (toString model.tickMs) ++ "ms")
                , input
                    [ type_ "range"
                    , value (toString model.tickMs)
                    , Html.Styled.Attributes.min "100"
                    , Html.Styled.Attributes.max "3000"
                    , Html.Styled.Attributes.step "100"
                    , onInput (\s -> SetTicks (Maybe.withDefault 500 (String.toFloat s)))
                    ] []
                ]
            , setting []
                [ text "Display process"
                , input [ type_ "checkbox", onCheck ToggleProcess, Html.Styled.Attributes.checked model.showParseResult ] []
                ]
            , setting []
                [ text "Arena"
                , select
                    [ onInput SetArena
                    , value model.arena
                    , css 
                        [ margin2 (px 4) (px 0)
                        , padding2 (px 4) (px 6)
                        ]
                    ]
                    [ option [value "beginner"] [text "Beginner vs. Bar"]
                    , option [value "prison"] [text "Prison vs. Baz"]
                    ]
                ]
            ]
        , actionRow []
            [ btn [ onClick RunPause ] [ text (if model.isRunning then "Pause" else "Run")]
            , btn [ onClick (RunStep False) ] [ text "Run Step" ] ]
    ]]) 
    |> toUnstyled

editor : Model -> List Style -> Html Msg
editor model style =
    div
        [ css ([ grid, property "grid-template-rows" "auto 1fr" ] ++ style) ]
        [ debugOutput
            [ css [ fontSize (Css.em 1.6), displayFlex, justifyContent center ]]
            [ text (listBots model.world.bots) ]
        , div
            [ css
            [ grid
            , property "grid-template-columns" (if model.showParseResult then "1fr 1fr" else "1fr")
            , property "grid-template-rows" "auto"
            , height (pct 100), overflow scroll
            ]]
            ((if model.showParseResult
                then [debugOutput
                [ css 
                [ overflow scroll
                , lineHeight (num 1.5)
                , order (num 1)
                , grid
                , property "grid-template-rows" "auto auto 1fr"
                ]]
                [ showPlayerBot model.world
                , hr [css [ margin (px 0), width (pct 110), transform (translateX (pct -5))]] []
                , text (Maybe.withDefault (showDebug model) <| model.world.error) ]]
                else []) ++
            [ textarea
            [ onInput UpdateScript
            , Html.Styled.Events.preventDefaultOn "beforeinput"
                    ((D.field "inputType" D.string) |> D.andThen (\i -> D.succeed (NOOP, model.modifier && i == "insertLineBreak")))
            , value model.script
            , placeholder "MOVE 1\nTURN RIGHT\nFIRE\nREPEAT 3 MOVE 1\nIF ENEMYAHEAD THEN FIRE 1 ELSE TURN LEFT\nWHILE NOT LOWHP DO SCAN\n..."
            , spellcheck False
            , css
                [ resize none
                , fontSize (Css.em 1.5)
                , padding (px 12)
                , textTransform uppercase
                , insetBorder
                , lineHeight (num 1.5)
                , margin (px 3)
                ]
            ] []
            ])
      , actionRow []
        [ btn [ onClick StoreScript ] [ text ("Upload to " ++ (getBotName model.world.bots)) ] ]
        ]

getBotName : List BotEntity -> String
getBotName bots = case bots of
    (bot :: _) -> bot.name
    [] -> "Unkown Bot"

renderRow : World -> Int -> Html Msg
renderRow world row =
    tr [] ((List.range 0 (Tuple.second world.arena.size))
        |> List.map (\i -> (row, i))
        |> List.map (renderCell world))

renderCell : World -> Coord -> Html Msg
renderCell world pos = td
    [ css
        [ padding (px 1)
        , textAlign center
        , border3 (px 1) solid theme.gridLines
        , size (px 50)
        ]
    ]
    [ case cell world pos of
        B b -> renderBot b world
        O _ -> renderWall
        Empty -> text ""
    ]

renderBot : BotEntity -> World -> Html Msg
renderBot bot w = div
    [ css [ transform (translateY (px 2))
    , fontSize (Css.em 1.8)
    , size (pct 100)
    , property "display" "grid"
    , alignItems center
    ]]
    [ Styles.pointer (toFloat bot.dirDeg)
    , healthBarBase theme.healthBarBackground 100
    , healthBarBase theme.healthBarForeground (100 * toFloat bot.hp / toFloat w.arena.maxHp)
    , text (if bot.alive then "🤖" else "🪦")
    ]

getObj : List Obj -> Coord -> Maybe Obj
getObj os c = case os of
    [] -> Maybe.Nothing
    (o :: oss) -> case o of
        Wall cc -> if c == cc then Just o else getObj oss c
        _ -> getObj oss c

getBot : List BotEntity -> Coord -> Maybe BotEntity
getBot bs c = case bs of
    [] -> Maybe.Nothing
    (b :: bss) -> if b.pos == c then Just b else getBot bss c

cell : World -> Coord -> Cell
cell world coord = case (getObj world.arena.objects coord) of
    Just o -> O o
    Maybe.Nothing -> case (getBot world.bots coord) of
        Just b -> B b
        _ -> Empty

showInstruction : Instr -> String
showInstruction i = case i of
    Move n -> "MOVE " ++ toString n
    Turn n -> "TURN " ++ toString n 
    Scan -> "SCAN"
    Fire -> "FIRE"
    Repeat n instr -> "REPEAT " ++ toString n ++ " " ++ showInstruction instr
    IfThenElse cond i1 i2 -> "IF (" ++ showCond cond ++ ") ? " ++ showInstruction i1 ++ " : " ++ showInstruction i2
    While cond instr -> "WHILE (" ++ showCond cond ++ ") " ++ showInstruction instr
    _ -> "?"

showCond : Cond -> String
showCond c = case c of
    EnemyAhead -> "ENEMY-AHEAD"
    LowHp -> "LOW-HP"
    WallAhead -> "WALL-AHEAD"
    Always -> "TRUE"
    Not inner -> "NOT " ++ showCond inner

showProgram : Int -> List Instr -> String
showProgram pc is = case is of
    [] -> "NO PROGRAM LOADED"
    _ -> is
        |> List.indexedMap (\i a -> (if i == pc then "→ " else " ") ++ showInstruction a)
        |> List.foldr (\a b -> a ++ "\n" ++ b) ""

showPlayerBot : World -> Html Msg
showPlayerBot w = case w.bots of
    (b :: _) ->  fieldset
        [ css
        [ margin3 (px -8) (px -8) (px 4)
        , border (px 0)
        , padding (px 0)
        , fontSize (Css.em 0.9)
        , grid
        , property "gap" "4px"
        ]]
        [ div [css[paddingLeft (px 2)]] [text b.name]
        , meter 
            [ value (toString b.hp)
            , Html.Styled.Attributes.max (toString w.arena.maxHp)
            , attribute "low" "3"
            , attribute "optimum" "10"
            , attribute "high" "8"
            ] []
        , div [css[paddingLeft (px 2), fontSize smaller]] [text ("PC " ++ (toString b.pc) ++ "\tPOS (" ++ (Tuple.first b.pos |> toString) ++ "," ++ (Tuple.second b.pos |> toString) ++ ")")]
        ]
    _ -> text "No INFO"

showDebug : Model -> String
showDebug { world } = case world.bots of
    (playerBot::_) -> String.join "\n" [(showProgram playerBot.pc playerBot.program)]
    _ -> ""

debugOutput : StyledElement msg
debugOutput = styled div
    [ padding (Css.em 1)
    , backgroundColor theme.debugBg
    , whiteSpace preWrap, fontFamily monospace
    , fontSize (Css.em 1.5)
    , outsetBorder
    ]
    
subscriptions : Model -> Sub Msg
subscriptions m = Sub.batch
    [ Events.onKeyDown (keyEventDecoder |> D.andThen (checkHotkey True))
    , Events.onKeyUp (keyEventDecoder |> D.andThen (checkHotkey False))
    , if (m.isRunning) then Time.every m.tickMs (\_ -> RunStep True) else Sub.none
    ]

keyEventDecoder : D.Decoder KeyEvent
keyEventDecoder =
    D.map5 KeyEvent
        (D.field "key" D.string)
        (D.field "ctrlKey" D.bool)
        (D.field "altKey" D.bool)
        (D.field "shiftKey" D.bool)
        (D.field "metaKey" D.bool)

checkHotkey : Bool -> KeyEvent -> D.Decoder Msg
checkHotkey _ keyEvent =
    D.succeed ((if keyEvent.shift then ModifierDown else ModifierUp) keyEvent)
