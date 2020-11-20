// Move caret on animation frame CodePen
// https://codepen.io/shawnlukas/pen/GRJZMOW

const zeroWidthSpace = "\u200B";

const isAndroid = () => {
  return /(android)/i.test(navigator.userAgent);
};

/**
 * Finds the selection path given the node, editor, and selection offset
 *
 * @param node - the DOM Node we're searching for
 * @param editor - a reference to the editor webcomponent
 * @param offset - the selection path offset if one exists
 * @returns {null|*[]} an array of indexes from the editor document root to the node, otherwise null.
 */
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

    // Drop the first two nodes
    indexPath.shift();
    indexPath.shift();

    return indexPath.slice();
  } catch (e) {
    // Sometimes we can get errors from trying to access properties like "tagName".  In that
    // just return null.
    return null;
  }
};

/**
 * Finds a node given a path.  Returns null if no node exists
 * @param path - an array of indexes or null
 * @param editor - a reference to the editor webcomponent
 * @returns {null|ChildNode|*} Returns the node this path refers to, null otherwise.
 */
const findNodeFromPath = (path, editor) => {
  if (!path) {
    return null;
  }

  if (typeof path === "string") {
    path = path.split(":").map((v) => Number(v));
  }

  let node = editor;
  let newPath = [0, 0, ...path];
  while (newPath.length && node) {
    let index = newPath.shift();
    node = node.childNodes && node.childNodes[index];
  }

  return node || null;
};


/**
 * Helper method to account for zeroWidthSpace and selection offsets that exceed the actual node
 * length.
 * @param node - a reference to the node this offset refers to
 * @param offset - the offset in question
 * @returns {number|*} the new offset
 */
let adjustOffsetReverse = (node, offset) => {
  if (node.nodeType === Node.TEXT_NODE && node.nodeValue === zeroWidthSpace) {
    return 1;
  }
  if (node.nodeType === Node.TEXT_NODE && offset > node.nodeValue.length) {
    return node.nodeValue.length;
  }
  return offset;
};

/**
 * Helper method to account for zeroWidthSpace and simplifying boundary selection by selecting
 * the child node and setting the offset to 0.  Note that this sometimes leads to invalid
 * selections, but I think it's better than the alternative of trying to derive the boundary logic
 * in Elm.
 *
 * @param node - a reference to the node this offset refers to
 * @param offset - the offset in question
 * @returns {number|*} the new offset
 */
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

function getCaretCoordinates() {
  let x = 0,
    y = 0;
  const isSupported = typeof window.getSelection !== "undefined";
  if (isSupported) {
    const selection = window.getSelection();
    // Check if there is a selection (i.e. cursor in place)
    if (selection.rangeCount !== 0) {
      // Clone the range
      const range = selection.getRangeAt(0).cloneRange();
      // Collapse the range to the start, so there are not multiple chars selected
      range.collapse(true);
      // getCientRects returns all the positioning information we need
      const rect = range.getClientRects()[0];
      if (rect) {
        x = rect.left; // since the caret is only 1px wide, left == right
        y = rect.top; // top edge of the caret
      }
    }
  }
  return {
    x,
    y
  };
}

function getCaretIndex(element) {
  let position = 0;
  const isSupported = typeof window.getSelection !== "undefined";
  if (isSupported) {
    const selection = window.getSelection();
    // Check if there is a selection (i.e. cursor in place)
    if (selection.rangeCount !== 0) {
      // Store the original range
      // Clone the range
      const range = window.getSelection().getRangeAt(0);
      const preCaretRange = range.cloneRange();

      // Select all textual contents from the contenteditable element
      // And set the range end to the original clicked position
      // Return the text length from contenteditable start to the range end
      preCaretRange.selectNodeContents(element);
      preCaretRange.setEnd(range.endContainer, range.endOffset);
      position = preCaretRange.toString().length;
    }
  }
  return position;
}


/**
 * SelectionState is a webcomponent that syncs the editor's selection state with the selection
 * API.
 */
class SelectionState extends HTMLElement {
  static get observedAttributes() {
    return ["selection"];
  }

  constructor() {
    super();
    this.selectionChange = this.selectionChange.bind(this);
  }

  attributeChangedCallback(name, oldValue, newValue) {
    if (name !== "selection") {
      return;
    }
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
      } catch (e) {
        // ignore selection errors
      }
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

  getSelectionObject() {
    const selection = {
      "selectionExists": false
    };
    const selectionObj = getSelection();
    if (!selectionObj) {
      return selection
    }
    const anchorPath = this.getSelectionPath(selectionObj.anchorNode, selectionObj.anchorOffset);
    const focusPath = this.getSelectionPath(selectionObj.focusNode, selectionObj.focusOffset);
    if (!anchorPath || !focusPath) {
      return selection
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

  selectionChange(e) {
    let selection = this.getSelectionObject(e);
    let event = new CustomEvent("editorselectionchange", {
      detail: selection
    });
    this.parentNode.dispatchEvent(event);
  };
}

/**
 * ElmEditor is the top level webcomponent responsible for enabling web APIs like clipboard
 * and mutation observers to be visible in Elm.  It uses custom events which are handled
 * by Elm event listeners.
 */
class ElmEditor extends HTMLElement {

  compositionStart() {
    this.composing = true;

    // Sometimes Android never fires compositionend... so we need to make sure it gets called
    // eventually.
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
    // Use a custom composition end function since in some browsers, it gets fired before
    // the last keydown event occurs.
    setTimeout(() => {
      if (!this.composing) {
        const newEvent = new CustomEvent("editorcompositionend", {
          detail: {}
        });
        this.dispatchEvent(newEvent);
      }
    }, 0)

  }

  constructor() {
    super();
    this.mutationObserverCallback = this.mutationObserverCallback.bind(this);
    this.pasteCallback = this.pasteCallback.bind(this);
    this._observer = new MutationObserver(this.mutationObserverCallback);
    this.addEventListener("paste", this.pasteCallback);
    this.addEventListener("compositionstart", this.compositionStart.bind(this));
    this.addEventListener("compositionend", this.compositionEnd.bind(this));
    this.dispatchInit = this.dispatchInit.bind(this);
    this.animationCallback();

    console.log("constructor completed");

  }

  static get observedAttributes() {
    return ['cursorindex'];
  }

  attributeChangedCallback(name, oldValue, newValue) {
    switch (name) {
      case 'cursorindex':
        console.log(`Value changed from ${oldValue} to ${newValue}`);
        break;
    }
  }

  animationCallback() {
    var element = this;

    requestAnimationFrame(function() {
      const index = getCaretIndex(element);
      const coord = getCaretCoordinates();
      const newEvent = new CustomEvent("caretposition", {
        detail: {
          index: index,
          x: coord.x,
          y: coord.y
        }
      });
      if (index != element.index) {
        element.dispatchEvent(newEvent);
      }
      element.index = index;

      element.animationCallback();
    });
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

  /**
   * Returns a list of selection path and text if all the mutations are characterData.  Otherwise
   * returns null.
   */
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
