const {
  Elm
} = require('../src/elm/Main.elm');

//require('./elm-editor.js')
require('./copy-dom.js')

const app = Elm.Main.init({
  node: document.getElementById('application')
});
