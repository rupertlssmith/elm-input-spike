const {
  Elm
} = require('../src/elm/Main.elm');

require('./elm-editable.js')

const app = Elm.Main.init({
  node: document.getElementById('application')
});
