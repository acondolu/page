import { type Rect } from "./Rect";
import { modBigInt, divBigInt } from "./Utils";
import { BLOCK_SIZE } from "./Constants";
import { Navigator } from "./Navigator";
import * as FontManager from "./FontManager";
import { CanvasRenderer } from "./CanvasRenderer";
import { TextCache } from "./TextCache";
import { Input } from "./Input";
import { Spinner } from "./Spinner";
import { Remote } from "./Remote";

export class Page extends EventTarget {
  navigator: Navigator;
  canvasRenderer: CanvasRenderer;
  textCache: TextCache;
  input: Input;
  spinner: Spinner;
  remote: Remote;

  // Relative paper coordinates
  // (relative to the bigint coords)
  cursorX: number = 0;
  cursorY: number = 0;
  // Last used column (for new lines)
  // Like cursorX, relative paper coordinate
  column: number = 0;

  constructor(token: string) {
    super();
    // Text Cache
    this.textCache = new TextCache();
    // CanvasRenderer
    const canvasElement = document.getElementById(
      "canvas",
    ) as HTMLCanvasElement;
    const inputElement = document.getElementById(
      "hiddenInput",
    ) as HTMLInputElement;
    this.canvasRenderer = new CanvasRenderer(canvasElement, inputElement);
    this.canvasRenderer.addEventListener("paint", () => this.paint());
    this.canvasRenderer.addEventListener("set-cursor", (event) => {
      const e = event as CustomEvent<{ cursorX: number; cursorY: number }>;
      this.onSetCursor(e.detail.cursorX, e.detail.cursorY);
    });
    this.canvasRenderer.addEventListener("move-rel", (event) => {
      const e = event as CustomEvent<{ x: number; y: number }>;
      this.remote.moveRelative(e.detail.x, e.detail.y);
    });
    // Input
    this.input = new Input(inputElement);
    this.input.addEventListener("write-char", (event) => {
      const e = event as CustomEvent<string>;
      this.onWriteChar(e.detail);
    });
    this.input.addEventListener("change-cursor", (event) => {
      const e = event as CustomEvent<
        (
          cursorX: number,
          cursorY: number,
          column: number,
        ) => [number, number, number]
      >;
      this.onChangeCursor(e.detail);
    });
    this.input.addEventListener("resize", (event) => {
      const e = event as CustomEvent<{ width: number; height: number }>;
      this.onResize(e.detail.width, e.detail.height);
    });
    // Navigator
    const navElement = document.getElementById("navigator") as HTMLDivElement;
    this.navigator = new Navigator(navElement);
    this.navigator.addEventListener("navigate-to", (event) => {
      const e = event as CustomEvent<{ x: bigint; y: bigint }>;
      this.onNavigateTo(e.detail.x, e.detail.y);
    });
    // Spinner
    this.spinner = new Spinner();
    // Remote
    this.remote = new Remote(token);
    this.remote.addEventListener("error", () => {
      this.spinner.fatal("disconnected");
      this.terminate();
    });
    this.remote.addEventListener("rect", (e) => {
      this.registerBlock((e as CustomEvent<Rect<string[]>>).detail);
    });
    this.remote.addEventListener("done", () => {
      this.spinner.stop();
    });
  }

  async init() {
    this.spinner.change(true);
    await FontManager.load();
    this.navigator.update(0, 0);
    this.input.init(); // this will also paint for the first time
  }

  terminate() {
    this.canvasRenderer.terminate();
    this.navigator.hide();
  }

  // Canvas Renderer

  private onSetCursor(cursorX: number, cursorY: number) {
    this.cursorX = cursorX;
    this.cursorY = cursorY;
    this.column = this.cursorX;
    this.paint();
  }

  // Input

  private onWriteChar(char: string) {
    this.remote.writeChar(this.cursorX, this.cursorY, char);
    this.textCache.writeChar(this.cursorX, this.cursorY, char);
    this.cursorX += 1;
    this.paint();
  }

  private onChangeCursor(
    modif: (
      cursorX: number,
      cursorY: number,
      column: number,
    ) => [number, number, number],
  ) {
    [this.cursorX, this.cursorY, this.column] = modif(
      this.cursorX,
      this.cursorY,
      this.column,
    );
    this.paint();
  }

  private onResize(width: number, height: number) {
    this.canvasRenderer.resize(width, height);
    this.remote.resize(width, height);
    this.paint();
    if (window.visualViewport!.height != window.innerHeight) {
      // This is supposed to happen when the mobile keyboard opens up.
      // scroll the cursor into view
      this.canvasRenderer.handleMobileKeyboard(this.cursorY);
    }
  }

  isPainting: boolean = false;
  private paint() {
    if (this.isPainting) return;
    this.isPainting = true;
    return window.requestAnimationFrame(() => {
      this.isPainting = false;
      this.canvasRenderer.draw(this.textCache, this.cursorX, this.cursorY);
      this.navigator.update(this.cursorX, this.cursorY);
    });
  }

  // Navigate to given absolute character coordinates.
  private async onNavigateTo(x: bigint, y: bigint) {
    const bigBlockSize = BigInt(BLOCK_SIZE);
    // these are the absolute char coordinates,
    // but we keep them a multiple of BLOCK_SIZE
    const ax = divBigInt(x, bigBlockSize) * bigBlockSize;
    const ay = divBigInt(y, bigBlockSize) * bigBlockSize;

    const cursorX = Number(modBigInt(x, bigBlockSize));
    const cursorY = Number(modBigInt(y, bigBlockSize));
    this.cursorX = cursorX;
    this.cursorY = cursorY;
    this.column = this.cursorX;
    this.canvasRenderer.setPosition(cursorX, cursorY);

    this.textCache.flush();
    this.navigator.setAbsolute(ax, ay);

    this.remote.moveAbsolute(ax, ay, cursorX, cursorY);
    this.paint();
  }

  private registerBlock(r: Rect<string[]>) {
    this.textCache.pushRect(r);
    return this.paint();
  }
}
