module View exposing (renderView, subscriptions)

import Debug exposing (toString)
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, spellcheck)
import Html.Styled.Events exposing (onClick, onInput)
import Model exposing (..)
import Styles exposing (..)
import Css.Global exposing (global, html, body)
import Json.Decode as D
import Browser.Events as Events

type Cell = B BotEntity | O Obj | Empty

renderView : Model -> Html.Html Msg
renderView model = (main_
    [ css [  mainLayout, fontFamilies [.value monospace] ] ]
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
                [ overflow scroll
                , lineHeight (num 1.5)
                ]
            ]
            [ text "" ]
        , actionRow [] [ btn [ onClick RunStep ] [ text "Run Step" ] ]
    ]]) 
    |> toUnstyled

mainLayout : Style
mainLayout = Css.batch 
    [ grid
    , property "grid-template" "'editor map' auto / 1fr auto"
    , padding (Css.em 1)
    , height (pct 100)
    , boxSizing borderBox
    ]

listBots : List { a | name : String } -> String
listBots bots = (bots |> List.map (\b -> b.name ) |> String.join " vs. ")

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

healthBarBase : Color -> Float -> Html msg
healthBarBase col w = div
    [ css [ position absolute
    , border3 (px 1) groove transparent
    , width (pct 100)
    , height (px 3)
    , top (px -2)
    , boxSizing borderBox
    , backgroundColor col
    , width (pct w)
    ]]
    []

pointer : Float -> Html Msg
pointer angle = div [ css
    [ position absolute
    , zIndex (int 0)
    , border3 (px 16) groove transparent
    , borderRightColor theme.directionPointer
    , borderTopColor theme.directionPointer
    , margin (px 0)
    , transforms [rotateZ (deg (angle - 90)), translateX (px 16), scale 0.3, rotateZ (deg 45)]
    , size (px 0)
    , boxSizing borderBox
    , left (calc (pct 50) minus (px 16))
    ]] []

renderBot : BotEntity -> World -> Html Msg
renderBot bot w = div
    [ css [ transform (translateY (px 2))
    , fontSize (Css.em 1.8)
    , size (pct 100)
    , property "display" "grid"
    , alignItems center
    ]]
    [ pointer (toFloat bot.dirDeg)
    , healthBarBase theme.healthBarBackground 100
    , healthBarBase theme.healthBarForeground (100 * toFloat bot.hp / toFloat w.arena.maxHp)
    , text (if bot.alive then "🤖" else "🪦")
    ]

renderWall : Html msg
renderWall = div
    [ css
    [ backgroundColor theme.wall
    , property "background-image" brickPattern
    , size (pct 100)
    , backgroundSize2 (px 20) (px 35)
    , opacity (num 0.95)
    , property "background-position" "0 0, 0 0, 10px 18px, 10px 18px, 0 0, 10px 18px"
    ]] []

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

renderRow : World -> Int -> Html Msg
renderRow world row =
    tr [] ((List.range 0 (Tuple.second world.arena.size))
        |> List.map (\i -> (row, i))
        |> List.map (renderCell world))

showInstruction : Instr -> String
showInstruction i = case i of
    Move n -> "MOVE " ++ toString n
    Turn n -> "TURN " ++ toString n ++ "°"
    Scan -> "SCAN"
    Fire x y -> "FIRE " ++ toString x ++ " " ++ toString y
    Repeat n instr -> "REPEAT " ++ toString n ++ " " ++ showInstruction instr
    IfThenElse cond i1 i2 -> "IF (" ++ showCond cond ++ ") ? " ++ showInstruction i1 ++ " : " ++ showInstruction i2
    While cond instr -> "WHILE (" ++ showCond cond ++ ") " ++ showInstruction instr
    _ -> "?"

showCond : Cond -> String
showCond c = case c of
    EnemyAhead -> "ENEMY-AHEAD"
    LowHp -> "LOW-HP"
    WallAhead -> "WALL-AHEAD"
    Not inner -> "NOT " ++ showCond inner

showProgram : Int -> List Instr -> String
showProgram pc is = case is of
    [] -> "NO PROGRAM LOADED"
    _ -> is
        |> List.indexedMap (\i a -> (if i == pc then "→ " else " ") ++ showInstruction a)
        |> List.foldr (\a b -> a ++ "\n" ++ b) ""

showDebug : Model -> String
showDebug { world } = case world.bots of
    (playerBot::_) -> (showProgram playerBot.pc playerBot.program)
    _ -> ""

editor : Model -> List Style -> Html Msg
editor model style =
    div
      [ css ([ grid, property "grid-template-rows" "auto 1fr" ] ++ style) ]
      [ debugOutput [ css [ fontSize (Css.em 1.6), displayFlex, justifyContent center ]] [ text (listBots model.world.bots) ]
        , div [css [grid, property "grid-template-columns" "1fr 1fr", property "grid-template-rows" "auto", height (pct 100), overflow scroll]]
            [ debugOutput
            [ css 
                [ overflow scroll
                , lineHeight (num 1.5)
                , order (num 1)
                ]
            ]
            [ text (showDebug model) ]
        , textarea
          [ onInput UpdateScript
          , Html.Styled.Events.preventDefaultOn "beforeinput"
                ((D.field "inputType" D.string) |> D.andThen (\i -> D.succeed (NOOP, model.modifier && i == "insertLineBreak")))
          , spellcheck False
          , css
              [ resize none
              , fontSize (Css.em 1.5)
              , padding (px 12)
              , textTransform uppercase
              , insetBorder
              , lineHeight (num 1.5)
              ]
          ] []
        ]
      , actionRow [] [ btn [ onClick StoreScript ] [ text "Save robo.script" ] ]
      ]

debugOutput : StyledElement msg
debugOutput = styled div
    [ padding (Css.em 1)
    , backgroundColor theme.debugBg
    , whiteSpace Css.pre, fontFamily monospace
    , fontSize (Css.em 1.5)
    , outsetBorder
    ]
    
subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch
    [ Events.onKeyDown (keyEventDecoder |> D.andThen (checkHotkey True))
    , Events.onKeyUp (keyEventDecoder |> D.andThen (checkHotkey False))
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
