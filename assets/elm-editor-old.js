class ElmEditor extends HTMLElement {

  constructor() {
    super();

    // this.pasteCallback = this.pasteCallback.bind(this);
    // this.addEventListener("paste", this.pasteCallback);

    // this.addEventListener("compositionstart", this.compositionStart.bind(this));
    // this.addEventListener("compositionend", this.compositionEnd.bind(this));

    //this.dispatchInit = this.dispatchInit.bind(this);
  }

  // dispatchInit() {
  //   if (!this.isConnected) {
  //     return;
  //   }
  //   const isMacLike = /(Mac|iPhone|iPod|iPad)/i.test(navigator.platform);
  //   const event = new CustomEvent("editorinit", {
  //     detail: {
  //       shortKey: isMacLike ? "Meta" : "Control"
  //     }
  //   });
  //   this.dispatchEvent(event);
  //   clearInterval(this.initInterval)
  // }

  // compositionStart() {
  //   this.composing = true;
  //
  //   if (isAndroid()) {
  //     if (this.lastCompositionTimeout) {
  //       clearTimeout(this.lastCompositionTimeout);
  //     }
  //     const lastCompositionTimeout = setTimeout(() => {
  //       if (this.composing && lastCompositionTimeout === this.lastCompositionTimeout) {
  //         this.composing = false;
  //         const newEvent = new CustomEvent("editorcompositionend", {
  //           detail: {}
  //         });
  //         this.dispatchEvent(newEvent);
  //       }
  //     }, 5000);
  //     this.lastCompositionTimeout = lastCompositionTimeout
  //   }
  // }
  //
  // compositionEnd() {
  //   this.composing = false;
  //
  //   setTimeout(() => {
  //     if (!this.composing) {
  //       const newEvent = new CustomEvent("editorcompositionend", {
  //         detail: {}
  //       });
  //       this.dispatchEvent(newEvent);
  //     }
  //   }, 0)
  // }

  // pasteCallback(e) {
  //   e.preventDefault();
  //
  //   const clipboardData = e.clipboardData || window.clipboardData;
  //   const text = clipboardData.getData('text') || "";
  //   const html = clipboardData.getData('text/html') || "";
  //   const newEvent = new CustomEvent("pastewithdata", {
  //     detail: {
  //       text: text,
  //       html: html
  //     }
  //   });
  //   this.dispatchEvent(newEvent);
  // }
}

// const isAndroid = () => {
//   return /(android)/i.test(navigator.userAgent);
// };
