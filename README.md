# Bot arena

Pure functional bot arena game with Game Queue and ACTIONS.

Current 2 Variants are evaluated:
1. TUI with brick in Haskell
2. GUI with ELM in Webbrowser rendering HTML tables

## 1
Build and Start with:
``` sh
cabal update
cabal build
cabal run
```

Create grid size dynamically with:
``` sh
cabal run bot-arena-tui -- 50 50 #row column
```

## 2
Build and Start with:
``` sh
elm make src/Main.elm --output=main.js
elm-live src/Main.elm --open -- --output=main.js
```