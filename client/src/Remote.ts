import { bigIntToHex } from "./Utils";
import * as Config from "./Config";
import { BLOCK_SIZE } from "./Constants";

/**
 * Messages sent to backend.
 */
type SendTy =
  | { tag: "move-relative"; x: number; y: number }
  | { tag: "move-absolute"; ax: string; ay: string; x: number; y: number }
  | { tag: "resize"; width: number; height: number }
  | { tag: "write-char"; x: number; y: number; c: string }
  | { tag: "ping" }
  | { tag: "renew-token"; token: string };

/**
 * Messages received from backend.
 */
type RecvTy =
  | { tag: "rect"; x: number; y: number; text: string[] }
  | { tag: "done" }
  | { tag: "pong" };

const DEBOUNCE_MILLIS: number = 100;

function debounce<T extends any[]>(
  func: (...args: T) => void,
): (...args: T) => void {
  let timeout: ReturnType<typeof setTimeout> | null = null;
  return (...args: T) => {
    if (timeout) return;
    timeout = setTimeout(() => {
      timeout = null;
      func.apply(null, args);
    }, DEBOUNCE_MILLIS);
  };
}

export class Remote extends EventTarget {
  private socket: WebSocket;
  private terminated: boolean = false;
  // Absolute coordinates, arbitrary precision
  private ax: bigint = 0n;
  private ay: bigint = 0n;
  // Relative coordinates (wrt absolute ones), fixed precision
  private rx: number = 0;
  private ry: number = 0;
  // Viewport size
  private width: number = 0;
  private height: number = 0;

  constructor(token: string) {
    super();
    const idempotencyKey = Config.idempotencyKey();
    // Token and idempotency key are used for authentication
    // with the backend server.
    this.socket = new WebSocket(Config.wsUrl(), [
      "page.acondolu.me",
      token,
      idempotencyKey,
    ]);
    this.socket.addEventListener("open", () => this.wsOnOpen());
    this.socket.addEventListener("close", (e) => this.wsOnClose(e));
    this.socket.addEventListener("error", (e) => this.wsOnError(e));
    this.socket.addEventListener("message", (e) => this.wsOnMessage(e));
  }

  terminate() {
    this.terminated = true;
    if (this.socket.readyState != WebSocket.CLOSED) {
      this.socket.close();
    }
  }

  moveRelative(x: number, y: number) {
    this.rx = x;
    this.ry = y;
    this._sendMoveRelative();
  }

  resize(width: number, height: number) {
    this.width = width;
    this.height = height;
    this._sendResize();
  }

  moveAbsolute(ax: bigint, ay: bigint, x: number, y: number) {
    this.ax = ax;
    this.ay = ay;
    this.rx = x;
    this.ry = y;
    this._sendMoveAbsolute();
  }

  ping() {
    this.send({ tag: "ping" });
  }

  writeChar(x: number, y: number, c: string) {
    // TODO: should support writing strings instead of characters,
    // to avoid calling the backend for each char. But it's for
    // another time.
    this.send({
      tag: "write-char",
      x,
      y,
      c,
    });
  }

  renewToken(token: string) {
    this.send({
      tag: "renew-token",
      token,
    });
  }

  // Debounced method implementations

  private _sendMoveRelative = debounce(() => {
    this.send({
      tag: "move-relative",
      x: this.rx,
      y: this.ry,
    });
  });

  private _sendMoveAbsolute = debounce(() => {
    this.send({
      tag: "move-absolute",
      ax: bigIntToHex(this.ax),
      ay: bigIntToHex(this.ay),
      x: this.rx,
      y: this.ry,
    });
  });

  private _sendResize = debounce(() => {
    this.send({
      tag: "resize",
      width: this.width,
      height: this.height,
    });
  });

  private send(x: SendTy) {
    if (this.socket.readyState != WebSocket.OPEN) return;
    return this.socket.send(JSON.stringify(x));
  }

  private schedulePing() {
    window.setTimeout(() => {
      this.ping();
    }, 2000);
  }

  // WebSocket events

  private wsOnOpen() {
    if (this.terminated) return;
    this.send({
      tag: "move-absolute",
      ax: bigIntToHex(this.ax),
      ay: bigIntToHex(this.ay),
      x: this.rx,
      y: this.ry,
    });
    this.send({
      tag: "resize",
      width: this.width,
      height: this.height,
    });
    this.schedulePing();
  }

  private wsOnClose(event: CloseEvent) {
    // if (this.terminated) return;
  }

  private wsOnError(event: Event) {
    if (this.terminated) return;
    console.error("WebSocket error", event, event.toString());
    this.dispatchEvent(new ErrorEvent("error", { message: event.toString() }));
  }

  private wsOnMessage(event: MessageEvent<string>) {
    if (this.terminated) return;
    const data: RecvTy = JSON.parse(event.data);
    switch (data.tag) {
      case "rect": {
        const r = {
          x1: data.x,
          x2: data.x + BLOCK_SIZE,
          y1: data.y,
          y2: data.y + BLOCK_SIZE,
          c: data.text,
        };
        this.dispatchEvent(new CustomEvent("rect", { detail: r }));
        return;
      }
      case "done": {
        this.dispatchEvent(new CustomEvent("done"));
        return;
      }
      case "pong": {
        this.schedulePing();
        return;
      }
      default: {
        const _: never = data;
        console.error("Remote.wsOnMessage", "unknown tag", data);
      }
    }
  }
}
