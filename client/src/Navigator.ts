export class Navigator extends EventTarget {
  private navElement: HTMLDivElement;

  // Absolute char coordinates
  private x: bigint;
  private y: bigint;

  constructor(navElement: HTMLDivElement) {
    super();
    this.navElement = navElement;
    this.x = 0n;
    this.y = 0n;
    this.navElement.addEventListener("click", (event) => this.onClick(event));
  }

  onClick(event: MouseEvent): void {
    event.preventDefault();
    event.stopPropagation();
    const s = prompt(
      "Insert coordinates separated by comma (format: 'X,Y', for example '-167,42'):",
    );
    if (!s || s.length < 3) return;

    const cs = s.split(",");
    const x = BigInt(cs[0]);
    const y = BigInt(cs[1]);

    this.dispatchEvent(
      new CustomEvent<{ x: bigint; y: bigint }>("navigate-to", {
        detail: {
          x,
          y,
        },
      }),
    );
  }

  setAbsolute(x: bigint, y: bigint) {
    this.x = x;
    this.y = y;
  }

  // Arguments are char coordinates relative to (this.x, this.y)
  update(x: number, y: number): void {
    const ret = [];

    const xstr = (this.x + BigInt(x)).toString();
    const ystr = (this.y + BigInt(y)).toString();
    const formattedX = xstr.startsWith("-") ? xstr : "+" + xstr;
    const formattedY = ystr.startsWith("-") ? ystr : "+" + ystr;

    const w =
      2 + Math.max(3 + formattedX.length, 3 + formattedY.length);

    // Upper border
    ret.push("┌");
    for (let i = 0; i < w; i++) ret.push("─");
    ret.push("┐\n");

    // X coordinate
    ret.push("│");
    ret.push(" X: ");
    ret.push(formattedX);
    for (let i = 4 + formattedX.length; i < w; i++) ret.push(" ");
    ret.push("│\n");

    // Y coordinate
    ret.push("│");
    ret.push(" Y: ");
    ret.push(formattedY);
    for (let i = 4 + formattedY.length; i < w; i++) ret.push(" ");
    ret.push("│\n");

    // Footer
    ret.push("└");
    for (let i = 0; i < w; i++) ret.push("─");
    ret.push("┘");

    this.navElement.innerText = ret.join("");
  }

  hide() {
    this.navElement.style.display = "none";
  }
}
