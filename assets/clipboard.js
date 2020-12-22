var ClipboardPorts = function() {};

ClipboardPorts.prototype.subscribe =
  function(app, copyPortName) {

    if (!copyPortName) copyPortName = "toClipboard";

    if (app.ports) {
      if (app.ports[copyPortName]) {
        app.ports[copyPortName].subscribe(function(val) {
          console.log("Copy port invoked.");
        });
      } else {
        console.error("The " + copyPortName + " port is not connected.");
      }
    } else {
      console.error("The Elm application has no ports defined.");
    }
  };

module.exports.ClipboardPorts = ClipboardPorts;
