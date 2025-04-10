export type Box = {
  x1: number;
  y1: number;
  x2: number;
  y2: number;
};

export type Rect<a> = {
  x1: number;
  y1: number;
  x2: number;
  y2: number;
  c: a;
};
