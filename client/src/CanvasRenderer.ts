import { rInvert, rMult, rToDouble } from "./Ratio";
import { metrics as fontMetrics } from "./FontManager";
import { TextCache } from "./TextCache";

const DEBUG = window.location.hash.includes("debug");

function eventCancel(e: Event) {
  if (e && e.preventDefault) {
    e.preventDefault();
  }
  if (e && e.stopPropagation) {
    e.stopPropagation();
  }
  return false;
}

// TODO:
// CSS pixels vs Screen pixels
// https://developer.mozilla.org/en-US/docs/Web/API/Window/devicePixelRatio

export class CanvasRenderer extends EventTarget {
  canvasElement: HTMLCanvasElement;
  inputElement: HTMLInputElement;

  // Viewport size
  width: number;
  height: number;

  // Color theme
  bgColor: string = "white";
  fgColor: string = "black";

  // We're in 2025, always assume a HiDPI/Retina display
  devicePixelRatio: number = 2;

  // Current screen position
  // (screen pixel coordinates, relative to bigint coords)
  x: number = 0;
  y: number = 0;

  // Scrolling
  scrolling: boolean = false;
  movementX: number;
  movementY: number;
  touchMoveHappened: boolean = false;
  previousTouch?: Touch;

  constructor(
    canvasElement: HTMLCanvasElement,
    inputElement: HTMLInputElement,
  ) {
    super();
    this.canvasElement = canvasElement;
    this.inputElement = inputElement;

    this.movementX = 0;
    this.movementY = 0;
    // will be updated by first resize event
    this.width = 0;
    this.height = 0;

    // dark mode support
    const q = window.matchMedia("(prefers-color-scheme: dark)");
    this.setDarkMode(q.matches);
    q.addEventListener("change", ({ matches }) => this.setDarkMode(matches));

    // Touch
    canvasElement.addEventListener("touchstart", (event) =>
      this.ontouchstart(event),
    );
    canvasElement.addEventListener("touchend", (event) =>
      this.ontouchend(event),
    );
    canvasElement.addEventListener("touchmove", (event) =>
      this.ontouchmove(event),
    );
    // Mouse
    canvasElement.addEventListener("click", (event) => this.onclick(event));
    canvasElement.addEventListener("mousedown", (event) =>
      this.scrollBegin(event),
    );
    canvasElement.addEventListener("mouseup", (event) => this.scrollEnd(event));
    canvasElement.addEventListener("mousemove", (event) =>
      this.scrollDo(event, event.movementX, event.movementY),
    );
    canvasElement.addEventListener("wheel", (event) => this.onWheel(event));

    canvasElement.addEventListener("focus", (e) => this.onfocus(e));
    canvasElement.addEventListener("blur", () => this.onblur());

    canvasElement.addEventListener("dragstart", eventCancel);
    canvasElement.addEventListener("selectstart", eventCancel);
  }

  terminate() {
    this.canvasElement.style.display = "none";
  }

  focus() {
    this.inputElement.focus();
  }

  resize(width: number, height: number) {
    if (DEBUG)
      console.log(
        "resize",
        window.visualViewport!.width,
        window.visualViewport!.height,
        window.innerHeight,
      );
    this.width = width;
    this.height = height;
  }

  setDarkMode(dark: boolean) {
    if (dark) {
      this.bgColor = "black";
      this.fgColor = "white";
    } else {
      this.bgColor = "white";
      this.fgColor = "black";
    }
    this.dispatchEvent(new CustomEvent("paint"));
  }

  draw(textCache: TextCache, cursorX: number, cursorY: number, drawCursor: boolean): void {
    const canvas = this.canvasElement;
    const width = this.width;
    const height = this.height;
    const bgColor = this.bgColor;
    const fgColor = this.fgColor;
    const font = fontMetrics;
    const cache = textCache.cache;
    const cachebb = textCache.cachebb;
    const x = this.x;
    const y = this.y;
    // Clear the canvas
    canvas.width = width * this.devicePixelRatio;
    canvas.height = height * this.devicePixelRatio;
    const ctx = canvas.getContext("2d", { alpha: true })!;
    ctx.scale(this.devicePixelRatio, this.devicePixelRatio);

    // Fill background
    ctx.fillStyle = bgColor;
    {
      const rx1 = rMult(cachebb.x1, font.width) - x;
      const rx2 = rMult(cachebb.x2, font.width) - x;
      const ry1 = rMult(cachebb.y1, font.height) - y;
      const ry2 = rMult(cachebb.y2, font.height) - y;
      const rx1n = rx1 < 0n ? 0 : rx1 > BigInt(width) ? width : Number(rx1);
      const rx2n = rx2 < 0n ? 0 : rx2 > BigInt(width) ? width : Number(rx2);
      const ry1n = ry1 < 0n ? 0 : ry1 > BigInt(height) ? height : Number(ry1);
      const ry2n = ry2 < 0n ? 0 : ry2 > BigInt(height) ? height : Number(ry2);
      ctx.fillRect(rx1n, ry1n, rx2n - rx1n, ry2n - ry1n);
    }

    const x1: number = x;
    const y1: number = y;
    const x2 = x1 + width;
    const y2 = y1 + height;

    ctx.fillStyle = fgColor;
    ctx.font = font.string;

    // Print all text in the cache (if visible)
    for (let r of cache) {
      // check if r is visible
      const rx1 = rMult(r.x1, font.width);
      const rx2 = rMult(r.x2, font.width);
      const ry1 = rMult(r.y1, font.height) + font.ascent;
      const ry2 = rMult(r.y2, font.height) + font.ascent;
      if (rx1 < x1 && rx2 < x1) continue;
      if (rx1 >= x2 && rx2 >= x2) continue;
      if (ry1 < y1 && ry2 < y1) continue;
      if (ry1 >= y2 && ry2 >= y2) continue;
      const dx = Number(rx1 - x1);
      let dy = Number(ry1 - y1);
      if (DEBUG) {
        ctx.save();
        ctx.strokeStyle = "red";
        ctx.strokeRect(dx, dy - font.ascent, rx2 - rx1, ry2 - ry1);
        ctx.restore();
      }
      let i = 0;
      for (let line of r.c) {
        ctx.fillText(line, dx, dy + rMult(i, font.height));
        i++;
      }
    }

    // Cursor
    if (drawCursor) {
      ctx.strokeStyle = "red";
      // ctx.strokeRect(0, 0, width-1, height-1);
      const cX = Number(rMult(cursorX, font.width) - x);
      const cY = rMult(cursorY, font.height) - y;
      ctx.strokeRect(
        cX,
        Number(cY),
        rToDouble(font.width),
        rToDouble(font.height),
      );
      ctx.fillStyle = "yellow";
      ctx.globalCompositeOperation = "multiply";
      ctx.fillRect(cX, cY, rToDouble(font.width), rToDouble(font.height));
    }

    // ctx.fillStyle = "gray";
    // if (x < 0) {
    //   ctx.fillRect(0, 0, -x, canvas.height);
    // } else {
    //   ctx.fillRect(canvas.width - x, 0, x, canvas.height);
    // }
    // if (y < 0) {
    //   ctx.fillRect(0, 0, canvas.width, -y);
    // } else {
    //   ctx.fillRect(0, canvas.height - y, canvas.width, y);
    // }
  }

  // Mouse events

  onclick(event: MouseEvent) {
    event.preventDefault();
    event.stopPropagation();
    this.setCursor(event);
  }

  onWheel(event: WheelEvent) {
    event.preventDefault();
    this.move(event.deltaX, event.deltaY);
    console.log(
      new CustomEvent<{ x: number; y: number }>("move-rel", {
        detail: {
          x: rMult(this.x, rInvert(fontMetrics.width)),
          y: rMult(this.y, rInvert(fontMetrics.height)),
        },
      }),
    );
    this.dispatchEvent(
      new CustomEvent<{ x: number; y: number }>("move-rel", {
        detail: {
          x: rMult(this.x, rInvert(fontMetrics.width)),
          y: rMult(this.y, rInvert(fontMetrics.height)),
        },
      }),
    );
    this.dispatchEvent(new CustomEvent("paint"));
  }

  move(dx: number, dy: number) {
    this.x += Math.floor(dx);
    this.y += Math.floor(dy);
  }

  // Touch events

  ontouchmove(event: TouchEvent) {
    const touch = event.touches[0];

    if (this.previousTouch) {
      const movementX = touch.pageX - this.previousTouch.pageX;
      const movementY = touch.pageY - this.previousTouch.pageY;
      if (movementX != 0 || movementY != 0) this.touchMoveHappened = true;
      console.log("ontouchmove", movementX, movementY);
      this.scrollDo(event, movementX, movementY);
    }
    this.previousTouch = touch;
  }

  ontouchstart(event: TouchEvent) {
    console.log("ontouchstart");
    this.previousTouch = event.touches[0];
    this.touchMoveHappened = false;
    this.scrollBegin(event);
  }

  ontouchend(event: TouchEvent) {
    event.preventDefault();
    event.stopPropagation();
    console.log("ontouchend", this.touchMoveHappened, this.previousTouch);
    if (!this.touchMoveHappened) {
      // This is a tap!
      if (this.previousTouch) this.setCursor(this.previousTouch);
      return;
    }
    return this.scrollEnd(event);
  }

  // Canvas blur / focus

  onfocus(event: FocusEvent) {
    event.preventDefault();
    event.stopPropagation();
    this.inputElement.focus();
  }

  onblur() {
    this.scrolling = false;
    this.movementX = 0;
    this.movementY = 0;
  }

  // Scrolling

  scrollBegin(event: Event) {
    event.preventDefault();
    event.stopPropagation();
    this.scrolling = true;
    this.movementX = 0;
    this.movementY = 0;
  }

  // Called by:
  // - onMouseUp
  // - onTouchEnd
  // - by onResize when mobile virtual keyboard appears
  scrollEnd(event?: Event) {
    if (event) event.preventDefault();
    this.scrolling = false;
    console.debug("scrollEnd", event);
    const f = () => {
      if (this.scrolling) return;
      this.move(-this.movementX, -this.movementY);
      this.movementX *= 0.93;
      this.movementY *= 0.93;
      this.dispatchEvent(new CustomEvent("paint"));
      if (Math.abs(this.movementX) < 2 && Math.abs(this.movementY) < 2) {
        // Scrolling is over: emit move event
        this.dispatchEvent(
          new CustomEvent<{ x: number; y: number }>("move-rel", {
            detail: {
              x: rMult(this.x, rInvert(fontMetrics.width)),
              y: rMult(this.y, rInvert(fontMetrics.height)),
            },
          }),
        );
        return;
      }
      window.setTimeout(f, 16);
    };
    window.setTimeout(f, 16);
  }

  // Move the viewport
  // Triggered by both mouseMove and touchMove
  scrollDo(event: Event, movementX: number, movementY: number) {
    if (!this.scrolling) return;
    event.preventDefault();
    this.move(-this.movementX, -this.movementY);
    this.movementX = movementX;
    this.movementY = movementY;
    this.dispatchEvent(new CustomEvent("paint"));
  }

  // onclick + ontouchend
  setCursor(event: { clientX: number; clientY: number }) {
    const x = event.clientX >> 0;
    const y = event.clientY >> 0;
    const cursorX = rMult(this.x + x, rInvert(fontMetrics.width));
    const cursorY = rMult(this.y + y, rInvert(fontMetrics.height));
    this.dispatchEvent(
      new CustomEvent<{ cursorX: number; cursorY: number }>("set-cursor", {
        detail: {
          cursorX,
          cursorY,
        },
      }),
    );
    this.inputElement.focus();
  }

  setPosition(x: number, y: number) {
    this.x = rMult(x, fontMetrics.width);
    this.y = rMult(y, fontMetrics.height);
  }

  handleMobileKeyboard(cursorY: number) {
    const cY = rMult(cursorY, fontMetrics.height) - this.y;
    if (cY >= window.visualViewport!.height) {
      this.movementY =
        (window.visualViewport!.height - window.innerHeight) * (1 / 0.93 - 1);
      this.movementX = 0;
      console.log(
        "my scroll",
        window.visualViewport!.height,
        window.innerHeight,
        this.movementX,
      );
      this.scrollEnd();
    }
  }
}
