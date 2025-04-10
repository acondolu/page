export class Input extends EventTarget {
  inputElement: HTMLInputElement;

  constructor(inputElement: HTMLInputElement) {
    super();
    this.inputElement = inputElement;
    inputElement.addEventListener("textInput", (e) =>
      this.onTextInput(e as InputEvent),
    );
    inputElement.addEventListener("keydown", (e) => this.onKeyDown(e));
    document
      .getElementById("hiddenForm")!
      .addEventListener("submit", (event) => this.onEnter(event));
    // Initialize size and handle resizing
    window.visualViewport!.addEventListener("resize", (event) =>
      this.onResize(event),
    );
  }

  init() {
    return this.onResize();
  }

  private onTextInput(event: InputEvent) {
    event.preventDefault();
    event.stopPropagation();
    const data = event.data;
    if (!data) return;
    console.log("input", event);
    this.inputElement.value = "";
    // if (event.inputType != 'insertCompositionText' && event.inputType != 'insertText') return false;
    if (new TextEncoder().encode(data).length > 1) {
      // Sorry, only ASCII supported :-(
      return false;
    }

    this.dispatchEvent(
      new CustomEvent<string>("write-char", {
        detail: data,
      }),
    );
    return false;
  }

  private onKeyDown(event: KeyboardEvent) {
    console.log("keydown", event.key);
    // event.preventDefault();
    // event.stopPropagation();
    let f: (
      cursorX: number,
      cursorY: number,
      column: number,
    ) => [number, number, number];
    switch (event.key) {
      case "ArrowRight":
        f = (x, y, c) => [x + 1, y, x + 1];
        break;
      case "ArrowLeft":
        f = (x, y, c) => [x - 1, y, x - 1];
        break;
      case "ArrowUp":
        f = (x, y, c) => [x, y - 1, c];
        break;
      case "ArrowDown":
        f = (x, y, c) => [x, y + 1, c];
        break;
      // case 'Enter':
      //   this.cursorY++; break;
      case "Backspace":
        f = (x, y, c) => [x - 1, y, c];
        break;
      default:
        return;
    }
    this.dispatchEvent(
      new CustomEvent<
        (
          cursorX: number,
          cursorY: number,
          column: number,
        ) => [number, number, number]
      >("change-cursor", { detail: f }),
    );
    return;
  }

  private onEnter(event: SubmitEvent) {
    event.preventDefault();
    event.stopPropagation();
    this.dispatchEvent(
      new CustomEvent<
        (
          cursorX: number,
          cursorY: number,
          column: number,
        ) => [number, number, number]
      >("change-cursor", { detail: (x, y, c) => [c, y + 1, c] }),
    );
    return false;
  }

  private onResize(event?: Event) {
    if (event) {
      event.preventDefault();
      event.stopPropagation();
    }
    const width = Math.floor(window.visualViewport!.width);
    const height = Math.floor(window.visualViewport!.height);
    this.dispatchEvent(
      new CustomEvent<{ width: number; height: number }>("resize", {
        detail: {
          width,
          height,
        },
      }),
    );
  }
}
