class SelectionState extends HTMLElement {
  static get observedAttributes() {
    //return ["selection"];
    return ['cursorrow', 'cursorcol'];
  }

  constructor() {
    super();
    this.selectionChange = this.selectionChange.bind(this);
  }

  attributeChangedCallback(name, oldValue, newValue) {
    switch (name) {
      case 'selection':
        setSelection;
        break;

      case 'cursorrow':
        this.cursorrow = newValue;
        setCaretPos(this.parentNode, this.cursorrow, this.cursorcol);
        break;

      case 'cursorcol':
        this.cursorcol = newValue;
        setCaretPos(this.parentNode, this.cursorrow, this.cursorcol);
        break;
    }
  }

  connectedCallback() {
    document.addEventListener("selectionchange", this.selectionChange)
  }

  disconnectedCallback() {
    document.removeEventListener("selectionchange", this.selectionChange)
  }

  getSelectionPath(node, offset) {
    return getSelectionPath(node, this.parentNode, offset)
  }

  findNodeFromPath(path) {
    return findNodeFromPath(path, this.parentNode)
  }

  setSelection() {
    let selectionObj = {};
    for (let pair of newValue.split(",")) {
      let splitPair = pair.split("=");
      if (splitPair.length === 2) {
        selectionObj[splitPair[0]] = splitPair[1]
      }
    }

    let focusOffset = Number(selectionObj["focus-offset"]);
    const focusNode = this.findNodeFromPath(selectionObj["focus-node"]);
    let anchorOffset = Number(selectionObj["anchor-offset"]);
    const anchorNode = this.findNodeFromPath(selectionObj["anchor-node"]);

    if (focusNode && anchorNode) {
      const sel = document.getSelection();

      anchorOffset = adjustOffsetReverse(anchorNode, anchorOffset);
      focusOffset = adjustOffsetReverse(focusNode, focusOffset);
      try {
        sel.setBaseAndExtent(anchorNode, anchorOffset, focusNode, focusOffset);
      } catch (e) {}
    }
  }

  getSelectionObject() {
    const noSelection = { "selectionExists": false };
    const selectionObj = getSelection();

    if (!selectionObj)
    {
      return noSelection;
    }
    else if (selectionObj.isCollapsed)
    {
      const focusPath = this.getSelectionPath(selectionObj.focusNode, selectionObj.focusOffset);
      if (!focusPath) { return noSelection; }
      const focusOffset = adjustOffset(selectionObj.focusNode, selectionObj.focusOffset);

      return {
        "selectionExists": true,
        "offset": focusOffset,
        "node": focusPath,
      }
    }
    else
    {
      const anchorPath = this.getSelectionPath(selectionObj.anchorNode, selectionObj.anchorOffset);
      const focusPath = this.getSelectionPath(selectionObj.focusNode, selectionObj.focusOffset);
      if (!anchorPath || !focusPath) { return noSelection; }
      const anchorOffset = adjustOffset(selectionObj.anchorNode, selectionObj.anchorOffset);
      const focusOffset = adjustOffset(selectionObj.focusNode, selectionObj.focusOffset);

      return {
        "selectionExists": true,
        "anchorOffset": anchorOffset,
        "focusOffset": focusOffset,
        "anchorNode": anchorPath,
        "focusNode": focusPath,
      }
    }
  }

  selectionChange(e) {
    let selection = this.getSelectionObject(e);
    let event = new CustomEvent("editorselectionchange", {
      detail: selection
    });
    this.parentNode.dispatchEvent(event);
  };
}

class ElmEditor extends HTMLElement {

  constructor() {
    super();
    this.mutationObserverCallback = this.mutationObserverCallback.bind(this);
    this.pasteCallback = this.pasteCallback.bind(this);
    this._observer = new MutationObserver(this.mutationObserverCallback);
    this.addEventListener("paste", this.pasteCallback);
    this.addEventListener("compositionstart", this.compositionStart.bind(this));
    this.addEventListener("compositionend", this.compositionEnd.bind(this));
    this.dispatchInit = this.dispatchInit.bind(this);
  }

  compositionStart() {
    this.composing = true;

    if (isAndroid()) {
      if (this.lastCompositionTimeout) {
        clearTimeout(this.lastCompositionTimeout);
      }
      const lastCompositionTimeout = setTimeout(() => {
        if (this.composing && lastCompositionTimeout === this.lastCompositionTimeout) {
          this.composing = false;
          const newEvent = new CustomEvent("editorcompositionend", {
            detail: {}
          });
          this.dispatchEvent(newEvent);
        }
      }, 5000);
      this.lastCompositionTimeout = lastCompositionTimeout
    }
  }

  compositionEnd() {
    this.composing = false;

    setTimeout(() => {
      if (!this.composing) {
        const newEvent = new CustomEvent("editorcompositionend", {
          detail: {}
        });
        this.dispatchEvent(newEvent);
      }
    }, 0)
  }

  connectedCallback() {
    this._observer.observe(this, {
      characterDataOldValue: true,
      attributeOldValue: false,
      attributes: true,
      childList: true,
      subtree: true,
      characterData: true
    });
    this.initInterval = setInterval(this.dispatchInit, 1000)
  }

  disconnectedCallback() {
    this._observer.disconnect();
  }

  pasteCallback(e) {
    e.preventDefault();

    const clipboardData = e.clipboardData || window.clipboardData;
    const text = clipboardData.getData('text') || "";
    const html = clipboardData.getData('text/html') || "";
    const newEvent = new CustomEvent("pastewithdata", {
      detail: {
        text: text,
        html: html
      }
    });
    this.dispatchEvent(newEvent);
  }

  caretCallback(e) {
    if (this.contains(event.target)) {
      const index = getCaretIndex(this);
      const coord = getCaretCoordinates();
      const newEvent = new CustomEvent("caretposition", {
        detail: {
          index: index,
          x: coord.x,
          y: coord.y
        }
      });
      this.dispatchEvent(newEvent);
    }
  }

  characterDataMutations(mutationsList) {
    if (!mutationsList) {
      return null;
    }

    let mutations = [];
    for (let mutation of mutationsList) {
      if (mutation.type !== "characterData") {
        return null;
      }
      mutations.push({
        path: getSelectionPath(mutation.target, this, 0),
        text: mutation.target.nodeValue
      });
    }
    return mutations;
  }

  mutationObserverCallback(mutationsList, _) {
    const element = this.querySelector('[data-rte-main="true"]');
    const selection = this.childNodes[1].getSelectionObject();

    const characterDataMutations = this.characterDataMutations(mutationsList);
    if (!!characterDataMutations) {
      const event = new CustomEvent("editorchange", {
        detail: {
          root: element,
          selection: selection,
          isComposing: this.composing,
          characterDataMutations: characterDataMutations,
          timestamp: (new Date()).getTime()
        }
      });
      this.dispatchEvent(event);
    }
  };

  dispatchInit() {
    if (!this.isConnected) {
      return;
    }
    const isMacLike = /(Mac|iPhone|iPod|iPad)/i.test(navigator.platform);
    const event = new CustomEvent("editorinit", {
      detail: {
        shortKey: isMacLike ? "Meta" : "Control"
      }
    });
    this.dispatchEvent(event);
    clearInterval(this.initInterval)
  }
}

customElements.define('elm-editor', ElmEditor);
customElements.define('selection-state', SelectionState);

const zeroWidthSpace = "\u200B";

const isAndroid = () => {
  return /(android)/i.test(navigator.userAgent);
};

const getSelectionPath = (node, editor, offset) => {
  const originalNode = node;
  if (!node) {
    return null;
  }

  let path = [];
  try {
    while (node && node.tagName !== "BODY") {
      path.push(node);
      if (node === editor) {
        break;
      }
      node = node.parentNode
    }

    if (path[path.length - 1] !== editor) {
      return null
    }

    let indexPath = [];
    for (let i = 0; i < path.length - 1; i += 1) {
      let child = path[i];
      let parent = path[i + 1];

      let index = 0;
      for (let childNode of parent.childNodes) {
        if (childNode === child) {
          break;
        }
        index += 1;
      }
      indexPath.unshift(index);
    }

    if (originalNode.nodeType === Node.ELEMENT_NODE && offset > 0) {
      indexPath.push(offset - 1)
    } else if (originalNode.nodeType === Node.ELEMENT_NODE && originalNode.childNodes[0]) {
      indexPath.push(0)
    }

    if (indexPath.length <= 2) {
      return null;
    }

    return indexPath.slice();
  } catch (e) {
    return null;
  }
};

const findNodeFromPath = (path, editor) => {
  if (!path) {
    return null;
  }

  if (typeof path === "string") {
    path = path.split(":").map((v) => Number(v));
  }

  let node = editor;
  let newPath = path;

  while (newPath.length && node) {
    let index = newPath.shift();
    node = node.childNodes && node.childNodes[index];
  }

  return node || null;
};

let adjustOffsetReverse = (node, offset) => {
  if (node.nodeType === Node.TEXT_NODE && node.nodeValue === zeroWidthSpace) {
    return 1;
  }
  if (node.nodeType === Node.TEXT_NODE && offset > node.nodeValue.length) {
    return node.nodeValue.length;
  }
  return offset;
};

let adjustOffset = (node, offset) => {
  if ((node.nodeType === Node.TEXT_NODE && node.nodeValue === zeroWidthSpace)) {
    return 0;
  }

  if (node.nodeType === Node.ELEMENT_NODE) {
    let childNode = node.childNodes[offset - 1];
    if (childNode && childNode.nodeType === Node.TEXT_NODE) {
      return (childNode.nodeValue || "").length
    }
  }

  return offset;
};

function setCaretPos(element, row, col) {
  var selection = window.getSelection();

  var range = document.createRange();
  node = findNodeFromPath([0, row, 0, 0], element);

  if (!!node) {
    range.setStart(node, col);
    range.collapse(true);
    selection.removeAllRanges();
    selection.addRange(range);
  }
};
