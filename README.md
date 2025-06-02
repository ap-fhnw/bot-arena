# Bot arena

A pure functional bot arena game with Game Queue and ACTIONS.

![](./bot-arena-ui.jpg)

This game is written in Elm, with three main parts:
1. **Parser**: Parses the user typed commands. The parsed commands are then stored in the bot's program queue.
2. **Interpreter**: The interpreter implements the bot instructions and updates the game state.
3. **View**: The view takes the game's state and renders it to the screen.

## Installation and Prerequsites

Install Elm-Live as described in [www.elm-live.com](https://www.elm-live.com/)

## Build and Run
Build and Start with:
``` sh
elm make src/Main.elm --output=main.js
elm-live src/Main.elm --open -- --output=main.js
```

## Gameplay

To interact with the bot, you need to type commands in the left input field. If all commands are valid you can see the parsed programm in the right field beside the input field.
- Click `Upload to Foo` to load the script to your bot.
- Click `Run` to execute the program.
- Click `Run Step` to execute one step of the program.

When running the program in `Auto-run` mode, you have the possiblity to define the `tick` speed in the `Tick` regulator. Moreover you are able to pause the game and resume it later by using the `Pause` / `Run` button.

### Commands

|Command          |Effect|
|-----------------------|------|
|`MOVE n`               |Moves the bot ahead by n in the current direction, stored in Model.BotEntity.dirDeg|
|`TURN dir`             |Turns the Bot direction `RIGHT\|LEFT\|AROUND\|STRAIGHT`|
|`FIRE`                 |Triggers the bot to fire in the direction it is looking|
|`SCAN`                 |Scans bot's surroundings in a radar-like manner|
|`REPEAT n instr`       |Repeats given `instr`, `n`-times|
|`WHILE cond DO instr`  |Executes `instr` as long as `cond` evaluates to `True`|
|`IF cond THEN instrA ELSE instrB` |Executes `instrA` if `cond` evaluates to `True`, otherwise executes `instrB`|

---
**Note**: `FIRE` shoots a bullet in the direction the bot is facing, and the bullet will travel until it hits a wall, an other bot or if it reaches the maxRange. One hit by a bullet will decrease the bot's HealthPoints by 2 (of max 10). Any change in this setting has to be done in the `Engine.elm` file at the moment.

---

#### TURN Directions

The `TURN` command uses the `TurnDir` data type specified in `Model.elm`. The bot can turn in four different directions, which are represented as follows:

|Direction |Effect|
|----------|------|
|`RIGHT`   |Turns the bot 90 degrees to the right|
|`LEFT`    |Turns the bot 90 degrees to the left|
|`AROUND`  |Turns the bot 180 degrees|
|`STRAIGHT`|Keeps the bot's direction unchanged|

#### Conditions

Conditions are defined as types in `Model.elm` and can be used in the `WHILE` and `IF` commands. 

|Condition        |Effect|
|-----------------|------|
|`WALLAHEAD`      |True if there is a wall in front of the bot|
|`ENEMYAHEAD`     |True if there is an enemy bot ahead|
|`LowHp`          |True if bot's HealthPoints are below `Max / 2`|
|`Always`         |Always evaluates to True|
|`Not Cond `      |Negates `Cond`|


---
**Note**: `WALLAHEAD`, and `ENEMYAHEAD` are limited by the bot's view range, which is set to 4 tiles at the moment.

---

#### Examples

```elm 
-- Move around the arena
WHILE NOT WALLAHEAD DO MOVE 1

-- Turn as long as there is no enemy ahead
WHILE NOT ENEMYAHEAD DO TURN RIGHT


```

## Outlook for future development

Though the game is functional, there are still many improvements and features that can be added. We located the following areas for future development:

- **Code-Blocks**: Incorporating whole code blocks in `REPEAT`, `WHILE`, and `IF` commands to allow for more complex bot behaviors - as of now, only single instructions are supported.
- **Bot-Execution-Stack**: Implementing a stack to manage the execution of bot commands, allowing for nested commands and better control flow.
- **FIRE Animation**: Adding a visual representation of the firing action, such as a bullet sprite that moves in the direction of the bot's facing.
