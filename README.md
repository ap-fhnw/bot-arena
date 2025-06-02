# Bot arena

A pure functional bot arena game with Game Queue and ACTIONS.

This game is written in Elm, with three main parts:
1. **Parser**: Parses the user typed commands. The parsed commands are then stored in the bot's program queue.
2. **Interpreter**: The interpreter implements the bot instructions and updates the game state.
3. **View**: The view takes the game's state and renders it to the screen.

## Installation and Prerequsites

TODO

## Build and Run
Build and Start with:
``` sh
elm make src/Main.elm --output=main.js
elm-live src/Main.elm --open -- --output=main.js
```

## Gameplay

### Commands

|Command          |Effect|
|-----------------------|------|
|`MOVE n`               |Moves the bot ahead by n in the current direction, stored in Model.BotEntity.dirDeg|
|`TURN dir`             |Turns the Bot direction `RIGHT\|LEFT\|AROUND\|STRAIGHT`|
|`FIRE`                 |Triggers the bot to fire in the direction it is looking|
|`SCAN`                 |Scans bot's surroundings in a radar-like manner|
|`REPEAT n instr`       |Repeats given `instr`, `n`-times|
|`WHILE cond DO instr`  ||
|`IF-THEN-ELSE`         ||


## Features which are planned
