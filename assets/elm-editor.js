class ElmEditor extends HTMLElement {

  constructor() {
    super();

    this.mutationObserverCallback = this.mutationObserverCallback.bind(this);
    this._observer = new MutationObserver(this.mutationObserverCallback);
    this.addEventListener("click", this.caretCallback.bind(this));
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
    const selection = getSelection(this);

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
  }

  caretCallback(e) {
    if (this.contains(event.target)) {
      const selection = getSelection(this);

      const newEvent = new CustomEvent("mouseselection", {
        detail: selection
      });

      this.dispatchEvent(newEvent);
    }
  }

}

class SelectionState extends HTMLElement {

  static get observedAttributes() {
    return ["selection"];
  }

  constructor() {
    super();
    this.selectionChange = this.selectionChange.bind(this);
  }

  connectedCallback() {
    document.addEventListener("selectionchange", this.selectionChange)
  }

  disconnectedCallback() {
    document.removeEventListener("selectionchange", this.selectionChange)
  }

  attributeChangedCallback(name, oldValue, newValue) {
    switch (name) {
      case 'selection':
        setSelection(this.parentNode, newValue);
        break;
    }
  }

  selectionChange(e) {
    console.log(e);
    let selection = getSelection(this.parentNode);
    let event = new CustomEvent("editorselectionchange", {
      detail: selection
    });

    this.parentNode.dispatchEvent(event);
  };
}

customElements.define('elm-editor', ElmEditor);
customElements.define('selection-state', SelectionState);

let getSelection = (node) => {
  const noSelection = {
    "selectionExists": false
  };
  const selectionObj = document.getSelection();

  if (!selectionObj) {
    return noSelection;
  } else if (selectionObj.isCollapsed) {
    const focusPath = getSelectionPath(selectionObj.focusNode, node, selectionObj.focusOffset)

    if (!focusPath) {
      return noSelection;
    }

    const focusOffset = adjustOffset(selectionObj.focusNode, selectionObj.focusOffset);

    return {
      "selectionExists": true,
      "offset": focusOffset,
      "node": focusPath,
    }
  } else {
    const anchorPath = getSelectionPath(selectionObj.anchorNode, node, selectionObj.anchorOffset)
    const focusPath = getSelectionPath(selectionObj.focusNode, node, selectionObj.focusOffset)

    if (!anchorPath || !focusPath) {
      return noSelection;
    }

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

let setSelection = (node, selectionDesc) => {
  let selectionObj = {};

  for (let pair of selectionDesc.split(",")) {
    let splitPair = pair.split("=");
    if (splitPair.length === 2) {
      selectionObj[splitPair[0]] = splitPair[1]
    }
  }

  let focusOffset = Number(selectionObj["focus-offset"]);
  const focusNode = findNodeFromPath(selectionObj["focus-node"], node)
  let anchorOffset = Number(selectionObj["anchor-offset"]);
  const anchorNode = findNodeFromPath(selectionObj["anchor-node"], node)

  if (focusNode && anchorNode) {
    const sel = document.getSelection();

    anchorOffset = adjustOffsetReverse(anchorNode, anchorOffset);
    focusOffset = adjustOffsetReverse(focusNode, focusOffset);

    try {
      sel.setBaseAndExtent(anchorNode, anchorOffset, focusNode, focusOffset);
    } catch (e) {}
  }
}


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

const zeroWidthSpace = "\u200B";

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
