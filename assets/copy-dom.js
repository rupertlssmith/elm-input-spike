class CopyDom extends HTMLElement {

  constructor() {
    super();

    this.animationCallback();

    this.counter = 0;

  }

  animationCallback() {
    var element = this;

    requestAnimationFrame(function() {
      element.animationCallback();

      var source = element.querySelector('[data-elm-dom]');

      var target = element.querySelector('[data-copy-dom]');
      while (target.firstChild) {
        target.removeChild(target.lastChild);
      }

      var children = source.children;
      for (var i = children.length - 1; i >= 0; i--) {
          var clone = children[i].cloneNode(true);
          target.appendChild(clone);
      }
    });
  }

}

customElements.define('copy-dom', CopyDom);
