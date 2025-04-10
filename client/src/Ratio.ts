/**
 * Ratio with two numeric components (a/b).
 */
export type Ratio = {
  a: number;
  b: number;
};

type Int = number;

// Inverse of a ratio
export function rInvert(r: Ratio): Ratio {
  return { a: r.b, b: r.a };
}

// Multiply a number by a ratio, return an integer.
export function rMult(n: number, r: Ratio): Int {
  return Math.floor((n * r.a) / r.b);
}

// Convert ratio to integer.
export function rToInt(r: Ratio): Int {
  return (r.a / r.b) >> 0;
}

// Convert ratio to a number.
export function rToDouble(r: Ratio): number {
  return r.a / r.b;
}
