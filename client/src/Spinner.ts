export class Spinner {
  private frames: string[] = ["|", "/", "─", "\\"];
  private elem: HTMLDivElement;
  private active: boolean | null = false;

  constructor() {
    this.elem = document.getElementById("spinner") as HTMLDivElement;
  }

  get isOn(): boolean {
    return this.active === true;
  }

  fatal(message?: string) {
    this.active = null;
    this.elem.innerText = `(x_x) ${message ? message : ""}`;
    this.elem.style.display = "flex";
    this.elem.style.color = "red";
  }

  change(state: boolean) {
    return state ? this.start() : this.stop();
  }

  stop() {
    this.active = null;
    this.elem.style.display = "none";
  }

  start() {
    if (this.active === true) return;
    if (this.active === null) {
      this.active = true;
      return;
    }
    this.active = true;
    this.elem.style.display = "flex";
    this.animate();
  }

  private animate() {
    if (!this.active) {
      if (this.active === null) this.active = false;
      return;
    }
    const i = Math.floor(Math.random() * this.frames.length);
    this.elem.innerText = "(^_^) Loading " + this.frames[i];
    window.setTimeout(() => requestAnimationFrame(() => this.animate()), 50);
  }
}
