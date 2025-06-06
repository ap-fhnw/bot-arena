module View exposing (renderView, subscriptions)

import Debug exposing (toString)
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, spellcheck, value, attribute)
import Html.Styled.Events exposing (onClick, onInput)
import Model exposing (..)
import Styles exposing (..)
import Css.Global exposing (global, html, body)
import Json.Decode as D
import Browser.Events as Events
import Time
import Html.Styled.Attributes exposing (placeholder)
import Components exposing (..)
import Svg.Styled.Attributes exposing (mode)

type Cell = B BotEntity | O Obj | Empty

renderView : Model -> Html.Html Msg
renderView model = (main_
    [ css
        [ displayGrid
        , property "grid-template" "'editor map' auto / 1fr auto"
        , padding (Css.em 1)
        , height (pct 100)
        , boxSizing borderBox
        , fontFamilies [.value monospace]
        ]
    ]
    [ global
        [ html [ property "color-scheme" "light dark" ]
        , body [ height (vh 100) ]
        ]
    , renderEditor model 
    , grid
        [ css
            [ property "grid-area" "map"
            , gridTemplateRows "auto 1fr auto"
            , Styles.scroll
            ]
        ]
        [ renderWorld model
        , renderSettings model    
        , actionRow []
            [ btn [ onClick RunPause ] [ text (if model.isRunning then "Pause" else "Run")]
            , btn [ onClick (RunStep False) ] [ text "Run Step" ]
            ]
        ]

    ]) 
    |> toUnstyled

renderSettings : Model -> Html Msg
renderSettings model =
    Styles.section
      [ css 
          [ displayFlex
          , flexDirection column
          , overflow Css.scroll
          , lineHeight (num 1.5)
          ]
      ]
      [ checkbox "Auto-load"
          model.autoLoad AutoLoad
      , checkbox "Auto-run"
          model.autoRun AutoRun
      , range ("Tick " ++ (toString model.tickMs) ++ "ms")
          (toString model.tickMs)
          (\s -> SetTicks (Maybe.withDefault 500 (String.toFloat s)))
          (100, 3000, 100)
      , checkbox "Display process"
          model.showParseResult ToggleProcess
      , Components.select "Arena"
          model.arena SetArena
          [ ("beginner", "vs. Bar (Beginner Map)")
          , ("prison", "vs. Baz (Prison)")
          , ("prisonc", "vs other")
          ]
      ]

renderWorld : Model -> Html Msg
renderWorld model =
    grid
        [ css
            [ gridTemplateCols ("repeat(" ++ (Tuple.second model.world.arena.size |> toString) ++ ", 1fr)")
            , gridTemplateRows ("repeat(" ++ (Tuple.first model.world.arena.size |> toString) ++ ", 1fr)")
            , property "gap" "0px"
            , property "align-content" "center"
            , margin auto
            --, border3 (px 2) solid (rgb 250 0 0)
            , width (px 500)
            , height (px 500)
            , outsetBorder
            ]
        ]
        (let (h, w) = model.world.arena.size
        in
        (List.repeat ((h) * (w)) (0, 0)
            |> List.indexedMap (\i _ -> (i // w, (modBy w i)))
                |> List.map (renderCell model.world)))

renderCell : World -> Coord -> Html Msg
renderCell world pos = td
    [ css
        [ padding (px 1)
        , textAlign center
        , border3 (px 1) solid theme.gridLines
        ]
    ]
    [ case cell world pos of
        B b -> renderBot b world
        O _ -> renderWall
        Empty -> text "" --((Tuple.first pos |> toString) ++ (Tuple.second pos |> toString) )
    ]

renderBot : BotEntity -> World -> Html Msg
renderBot bot w = div
    [ css [ transform (translateY (px 2))
    , fontSize (Css.em (20 / (toFloat (Tuple.first w.arena.size))))
    , size (pct 100)
    , property "display" "grid"
    , alignItems center
    ]]
    [ Components.pointer (toFloat bot.dirDeg)
    , healthBarBase theme.healthBarBackground 100
    , healthBarBase theme.healthBarForeground (100 * toFloat bot.hp / toFloat w.arena.maxHp)
    , text (if bot.alive then "🤖" else "🪦")
    ]

renderWall : Html msg
renderWall = div
    [ css
        [ backgroundColor theme.wall
        , borderColor theme.wallBorder
        , outsetBorder
        , size (pct 100)
        ]
    ] []

renderEditor : Model -> Html Msg
renderEditor model = grid
    [ css
        [ gridArea "editor"
        , gridTemplateRows "auto 1fr"
        , gap (px 8)
        , Styles.scroll
        ]
    ]
    [ Styles.section
        [ css [ fontSize (Css.em 1.6), displayFlex, justifyContent center ]]
        [ text (listBots model.world.bots) ]
    , grid
        [ css
            [ gridTemplateCols (if model.showParseResult then "1fr 1fr" else "1fr")
            , gridTemplateRows "auto"
            , Styles.scroll
            ]
        ]
        ((if model.showParseResult then [renderSidebar model] else [])
            ++ [renderTextarea model])
    , actionRow []
    [ btn [ onClick StoreScript ] [ text ("Upload to " ++ (getBotName model.world.bots)) ] ]
    ]


renderTextarea : Model -> Html Msg
renderTextarea model =
    textarea
        [ onInput UpdateScript
        , Html.Styled.Events.preventDefaultOn "beforeinput"
                ((D.field "inputType" D.string) |> D.andThen (\i -> D.succeed (NOOP, model.modifier && i == "insertLineBreak")))
        , value model.script
        , placeholder "MOVE 1\nTURN RIGHT\nFIRE 1\nREPEAT 3 MOVE 1\nIF ENEMYAHEAD THEN FIRE 1 ELSE TURN LEFT\nWHILE NOT LOWHP DO SCAN\n..."
        , spellcheck False
        , css
            [ resize none
            , fontSize (Css.em 1.5)
            , padding (px 12)
            , textTransform uppercase
            , insetBorder
            , lineHeight (num 1.5)
            , margin (px 1)
            ]
        ] []

renderSidebar : Model -> Html Msg
renderSidebar model =
    Styles.section
        [ css 
            [ displayGrid
            , overflow Css.scroll
            , lineHeight (num 1.5)
            , width (pct 100)
            , order (num 1)
            , gridTemplateRows "auto auto 1fr"
            ]
        ]
        [ showPlayerBotInfo model.world
        , text (Maybe.withDefault (showDebug model) model.world.error)
        ]

listBots : List { a | name : String } -> String
listBots bots = bots
    |> List.map (\b -> b.name )
    |> String.join " vs. "

getBotName : List BotEntity -> String
getBotName bots = case bots of
    (bot :: _) -> bot.name
    [] -> "Unkown Bot"

getObj : List Obj -> Coord -> Maybe Cell
getObj os c = os |> List.filterMap
    (\o -> case o of
        Wall cc -> if cc == c then Just (O o) else Nothing
        _ -> Nothing
    ) |> List.head

getBot : List BotEntity -> Coord -> Maybe Cell
getBot bs c = bs
    |> List.filter (\b -> b.pos == c)
    |> List.head |> Maybe.andThen (Just << B)

cell : World -> Coord -> Cell
cell world coord = oneOf
    (getBot world.bots coord)
    (getObj world.arena.objects coord)

oneOf : Maybe Cell -> Maybe Cell -> Cell
oneOf a b = a |> Maybe.withDefault (b |> Maybe.withDefault Empty)

showProgram : Int -> List Instr -> String
showProgram pc is = case is of
    [] -> "NO PROGRAM LOADED"
    _ -> is
        |> List.indexedMap (\i a -> (if i == pc then "→ " else " ") ++ showInstruction a)
        |> List.foldr (\a b -> a ++ "\n" ++ b) ""

showInstruction : Instr -> String
showInstruction i = case i of
    Move n -> "MOVE " ++ toString n
    Turn n -> "TURN " ++ toString n 
    Scan -> "SCAN"
    Fire -> "FIRE"
    Repeat n instr -> "REPEAT " ++ toString n ++ " " ++ showInstruction instr
    IfThenElse cond i1 i2 -> "IF (" ++ showCond cond ++ ") ? " ++ showInstruction i1 ++ " : " ++ showInstruction i2
    While cond instr -> "WHILE (" ++ showCond cond ++ ") " ++ showInstruction instr
    Seq instrs -> "[ " ++ String.join "; " (List.map showInstruction instrs) ++ " ]"
    _ -> "?"

showCond : Cond -> String
showCond c = case c of
    EnemyAhead -> "ENEMY-AHEAD"
    LowHp -> "LOW-HP"
    WallAhead -> "WALL-AHEAD"
    Always -> "TRUE"
    Not inner -> "NOT " ++ showCond inner

showPlayerBotInfo : World -> Html Msg
showPlayerBotInfo w = case w.bots of
    (b :: _) -> div
        [ css
            [ padding (Css.rem 0.5)
            , backgroundColor theme.debugBg
            , whiteSpace preWrap, fontFamily monospace
            , outsetBorder,
                margin3 (px -16) (px -16) (px 4)
            , fontSize (Css.em 0.9)
            , displayGrid
            , gap (px 4)
            ]
        ]
        [ div
            [ css [ paddingLeft (px 2) ] ]
            [ text b.name ]
        , meter 
            [ (value << toString) b.hp
            , (Html.Styled.Attributes.max << toString) w.arena.maxHp
            , attribute "low" "3"
            , attribute "optimum" "10"
            , attribute "high" "8"
            ] []
        , div
            [ css
                [ paddingLeft (px 2)
                , fontSize smaller]
            ]
            [ text ("PC " ++ (toString b.pc)
                ++ "\tPOS (" ++ (Tuple.first b.pos |> toString) ++ "," ++ (Tuple.second b.pos |> toString) ++ ")")
            ]
        ]
    _ -> text "No INFO"

showDebug : Model -> String
showDebug { world } = case world.bots of
    (playerBot::_) -> String.join "\n" [(showProgram playerBot.pc playerBot.program)]
    _ -> ""
    
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
