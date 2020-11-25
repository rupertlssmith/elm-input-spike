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

      var clone = element.firstChild.cloneNode(true);
      clone.removeAttribute("hidden");

      if (element.childElementCount > 1)
          element.removeChild(element.lastChild);

      element.appendChild(clone);
    });
  }

}

customElements.define('copy-dom', CopyDom);
