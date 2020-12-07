class ElmEditable extends HTMLElement {

  constructor() {
    super();

    this.mutationObserverCallback = this.mutationObserverCallback.bind(this);
    this._observer = new MutationObserver(this.mutationObserverCallback);
    this.selectionChange = this.selectionChange.bind(this);
    this.mousedownCallback = this.mousedownCallback.bind(this);
    this.mouseupCallback = this.mouseupCallback.bind(this);

    this.mousedown = false;
  }

  connectedCallback() {
    this.addEventListener("mousedown", this.mousedownCallback);
    this.addEventListener("mouseup", this.mouseupCallback);
    document.addEventListener("selectionchange", this.selectionChange);

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
    this.removeEventListener("mousedown", this.mousedownCallback);
    this.removeEventListener("mouseup", this.mouseupCallback);
    document.removeEventListener("selectionchange", this.selectionChange)
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
    const selection = getSelection(this);

    const characterDataMutations = this.characterDataMutations(mutationsList);

    if (!!characterDataMutations) {
      const event = new CustomEvent("textchange", {
        detail: {
          selection: selection,
          isComposing: this.composing,
          characterDataMutations: characterDataMutations,
          timestamp: (new Date()).getTime()
        }
      });

      this.dispatchEvent(event);
    }
  }

  selectionChange(e) {
    let selection = getSelection(this);

    let isControlEvent = this.mousedown;

    let event = new CustomEvent("selectionchange", {
      detail: {
        selection: selection,
        ctrlEvent: isControlEvent,
        timestamp: (new Date()).getTime()
      }
    });

    this.dispatchEvent(event);
  };


  mousedownCallback(e) {
    console.log("mousedown");
    this.mousedown = true;
  }

  mouseupCallback(e) {
    console.log("mouseup");
    this.mousedown = false;
  }
}

class SelectionHandler extends HTMLElement {

  constructor() {
    super();
  }

  set selection(newValue) {
    if (!equalSelection(this.sel, newValue)) {
      this.sel = newValue;
      setSelection(this.parentNode, newValue);
    }
  }

  get selection() {
    return this.sel;
  }
}

customElements.define('elm-editable', ElmEditable);
customElements.define('selection-handler', SelectionHandler);


let getSelection = (node) => {
  const noSelection = {
    "selection": "noselection"
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
      "selection": "collapsed",
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
      "selection": "range",
      "anchorOffset": anchorOffset,
      "focusOffset": focusOffset,
      "anchorNode": anchorPath,
      "focusNode": focusPath,
    }
  }
}

let setSelection = (node, selection) => {
  let focusOffset;
  let focusNode;

  let anchorOffset;
  let anchorNode;

  if ("noselection" == selection.selection) {
    return;
  } else if ("collapsed" == selection.selection) {
    focusOffset = selection.offset;
    focusNode = findNodeFromPath(selection.node, node)

    anchorOffset = focusOffset;
    anchorNode = focusNode;
  } else if ("range" == selection.selection) {
    focusOffset = selection.focusOffset;
    focusNode = findNodeFromPath(selection.focusNode, node);

    anchorOffset = selection.anchorOffset;
    anchorNode = findNodeFromPath(selection.anchorNode, node);
  }

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

  let node = editor;
  let newPath = path.concat(); // Copies array, original not mutated.

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

// Returns 'true' if two selection objects are the same.
let equalSelection = (sel1, sel2) => {
  if (!sel1)
    return false;

  if (!sel2)
    return false;

  if (sel1.selection != sel2.selection)
    return false;

  if (sel1.selection == "collapsed") {
    if (sel1.offset != sel2.offset)
      return false;

    if (!equalArray(sel1.node, sel2.node))
      return false;
  }

  if (sel1.selection == "range") {
    if (sel1.anchorOffset != sel2.anchorOffset)
      return false;

    if (sel1.focusOffset != sel2.focusOffset)
      return false;

    if (!equalArray(sel1.focusNode, sel2.focusNode))
      return false;

    if (!equalArray(sel1.anchorNode, sel2.anchorNode))
      return false;
  }

  return true;
}

// Returns 'true' if two arrays are the same by element value.
// Does not support nested arrays or deep comparison of elements.
let equalArray = (arr1, arr2) => {
  if (!arr1)
    return false;

  if (!arr2)
    return false;

  if (arr1.length != arr2.length)
    return false;

  for (var i = 0, l = arr1.length; i < l; i++) {
    if (arr1[i] != arr2[i])
      return false;
  }

  return true;
}
