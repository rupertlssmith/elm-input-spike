const {
  Elm
} = require('../src/elm/Main.elm');

import '@webcomponents/webcomponentsjs/webcomponents-bundle.js'
require('./elm-editable.js')

const app = Elm.Main.init({
  node: document.getElementById('application')
});
