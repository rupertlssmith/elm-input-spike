const {
  Elm
} = require('../src/elm/Main.elm');

import '@webcomponents/webcomponentsjs/webcomponents-bundle.js'
require('./elm-editable.js');
const ClipboardPorts = require('./clipboard.js').ClipboardPorts;

const app = Elm.Main.init({
  node: document.getElementById('application')
});

// Subscribe to the clipboard ports.
const clipboard = new ClipboardPorts();
clipboard.subscribe(app);
