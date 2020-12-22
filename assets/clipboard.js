var ClipboardPorts = function() {};

ClipboardPorts.prototype.subscribe =
  function(app, copyPortName) {

    if (!copyPortName) copyPortName = "toClipboard";

    if (app.ports) {
      if (app.ports[copyPortName]) {
        app.ports[copyPortName].subscribe(function(val) {
          copyTextToClipboard(val);
        });
      } else {
        console.error("The " + copyPortName + " port is not connected.");
      }
    } else {
      console.error("The Elm application has no ports defined.");
    }
  };

module.exports.ClipboardPorts = ClipboardPorts;

function copyTextToClipboard(text) {
  if (!navigator.clipboard) {
    console.error('No navigator.clipboard, could not copy text.');
    return;
  }

  navigator.clipboard.writeText(text).then(function() {}, function(err) {
    console.error('Could not copy text: ', err);
  });
}
