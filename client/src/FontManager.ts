import { type Ratio } from "./Ratio";

export interface FontMetrics {
  height: Ratio;
  width: Ratio;
  ascent: number;
  size: number;
  family: string;
  string: string;
}

export const metrics: FontMetrics = (() => {
  let sizeFromComputedStyle: string = window
    .getComputedStyle(document.body, null)
    .getPropertyValue("font-size");
  const fontSize = parseFloat(sizeFromComputedStyle) ?? 10;
  const fontFamily = "SourceCodePro";
  const fAscent = 984;
  const fDescent = -273;
  const fW = 600;
  const fH = fAscent - fDescent; // 1257

  const unitsPerEm = 1000;
  const fontHeight = {
    a: fH * fontSize,
    b: unitsPerEm,
  };
  const fontWidth = {
    a: fW * fontSize,
    b: unitsPerEm,
  };
  const ascent = Math.floor((fAscent * fontSize) / unitsPerEm);

  return {
    height: fontHeight,
    width: fontWidth,
    ascent,
    size: fontSize,
    family: fontFamily,
    string: `${fontSize}px ${fontFamily}`,
  };
})();

export async function load() {
  await document.fonts.load(
    `${metrics.size}px "./SourceCodePro-Medium.ttf.woff2"`,
  );
}
