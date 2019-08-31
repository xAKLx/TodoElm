# TodoElm

Elm implementation of a simple TODO list.
Design: [Task Management](https://dribbble.com/shots/6712828-Task-Management)

## To run it
- Install Elm (0.19.0) from [Elm Releases](https://github.com/elm/compiler/releases).
- Run the following commands:
```
npm i
elm make src/Main.elm --output=dist/elm.js --optimize
npm run build
```

## For Development
```
elm make src/Main.elm --output=dist/elm.js # run this everytime you change .elm files
npm run sass:watch
npm run webpack:watch
```