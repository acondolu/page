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

  private animate(lastTime = 0) {
    requestAnimationFrame((time) => {
      if (!this.active) {
        if (this.active === null) this.active = false;
        return;
      }
      // ~20fps
      if (time - lastTime >= 50) {
        const i = Math.floor(Math.random() * this.frames.length);
        this.elem.innerText = "(^_^) Loading " + this.frames[i];
        lastTime = time;
      }
      this.animate(lastTime);
    });
  }
}
