import { BLOCK_SIZE } from "./Constants";
import { Box, Rect } from "./Rect";
import { div, replaceAt } from "./Utils";

const EMPTY_LINE: string = new Array(BLOCK_SIZE + 1).join(" ");

export class TextCache {
  cache: Rect<string[]>[];
  cachebb: Box;
  constructor() {
    this.cache = [];
    this.cachebb = { x1: 0, y1: 0, x2: 0, y2: 0 };
  }

  pushRect(r: Rect<string[]>) {
    for (let r2 of this.cache) {
      if (r.x1 == r2.x1 && r.y1 == r2.y1) {
        // Ovewrite the text in the cache
        r2.c = r.c;
        return;
      }
    }
    this.cache.push(r);
  }

  flush() {
    this.cache = [];
    // FIXME: what to do with cachebb ?
  }

  private newBlock(x: number, y: number): Rect<any[]> {
    const kx = div(x, BLOCK_SIZE) * BLOCK_SIZE;
    const ky = div(y, BLOCK_SIZE) * BLOCK_SIZE;
    const c = new Array(BLOCK_SIZE);
    for (let i = 0; i < BLOCK_SIZE; i++) c[i] = EMPTY_LINE;
    const new_rect = {
      x1: kx,
      x2: kx + BLOCK_SIZE,
      y1: ky,
      y2: ky + BLOCK_SIZE,
      c,
    };
    console.debug("newBlock", x, y, new_rect);
    this.cache.push(new_rect);
    return new_rect;
  }

  writeChar(x: number, y: number, c: string) {
    // Try and find an existing block
    for (let r of this.cache) {
      if (r.x1 <= x && x < r.x2 && r.y1 <= y && y < r.y2) {
        r.c[y - r.y1] = replaceAt(r.c[y - r.y1], x - r.x1, c);
        return;
      }
    }
    const r = this.newBlock(x, y);
    r.c[y - r.y1] = replaceAt(r.c[y - r.y1], x - r.x1, c);
  }
}
