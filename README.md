# hawaii
the hawaii game but in elm instead of react

Use an installer for [Elm](https://guide.elm-lang.org/install.html) or run:
```sh
npm install -g elm elm-live
```
That will install Elm globally along with [Elm Live](https://github.com/wking-io/elm-live) which is a
very handy live reloader.


Install the Elm dependencies and compile:
```sh
elm make src/Main.elm --output index.js
```

To install the Node dependencies, cd into `./server` and run:
```sh
npm install
```
or
```sh
yarn
```

To run the JSON API while still in `./server`:
```sh
npm start
```
or
```sh
yarn start
```

To develop:
```sh
elm-live src/Main.elm --open -- --output=index.js
```

To run the optimizer:
```sh
npm install uglify-js --global
chmod +x ./optimize.sh
./optimize.sh
```
