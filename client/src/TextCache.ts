import { Box, Rect } from "./Rect";
import { replaceAt } from "./Utils";

export class TextCache {
  cache: Rect<string[]>[];
  local: Rect<string[]>[];
  cachebb: Box;
  constructor() {
    this.cache = [];
    this.local = [];
    this.cachebb = { x1: 0, y1: 0, x2: 0, y2: 0 };
  }

  pushRect(r: Rect<string[]>) {
    let doPush = true;
    for (let r2 of this.cache) {
      if (r.x1 == r2.x1 && r.y1 == r2.y1) {
        // Ovewrite the text in the cache
        r2.c = r.c;
        doPush = false;
        break;
      }
    }
    if (doPush) this.cache.push(r);
    // Check local
    const local = this.local;
    let rl;
    for (let ix = 0; ix < local.length; ) {
      rl = local[ix];
      const x = rl.x1,
        y = rl.y1;
      if (r.x1 <= x && x < r.x2 && r.y1 <= y && y < r.y2) {
        if (rl.c[0] == r.c[y - r.y1].charAt(x - r.x1)) {
          this.local.splice(ix, 1);
          continue;
        }
      }
      ix++;
    }
  }

  flush() {
    this.cache = [];
    // FIXME: what to do with cachebb ?
  }

  private newBlock(x: number, y: number, c: string): Rect<any[]> {
    const new_rect = {
      x1: x,
      x2: x + 1,
      y1: y,
      y2: y + 1,
      c: [c],
    };
    console.debug("newBlock", x, y, new_rect);
    this.local.push(new_rect);
    return new_rect;
  }

  writeChar(x: number, y: number, c: string) {
    // Try and erase from cached blocks
    for (let r of this.cache) {
      if (r.x1 <= x && x < r.x2 && r.y1 <= y && y < r.y2) {
        r.c[y - r.y1] = replaceAt(r.c[y - r.y1], x - r.x1, " ");
        break;
      }
    }
    // Try and find an existing block
    for (let r of this.local) {
      if (r.x1 == x && r.y1 == y) {
        r.c = [c];
        return;
      }
    }
    this.newBlock(x, y, c);
  }
}
