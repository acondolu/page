import { Page } from "./Page";
import * as Config from "./Config";

let p: Page | undefined;

async function main(token: string) {
  if (p) {
    // This function is called by the Turnstile
    // callback when refreshing the token.
    // In that case, page has already been initialized.
    p.remote.newToken(token);
    return;
  }
  p = new Page(token);
  await p.init();
}

(window as any).onloadTurnstileCallback = function () {
  const modal = document.getElementById("captcha") as HTMLElement;
  (window as any).turnstile.render("#cf-turnstile", {
    sitekey: Config.CF_TURNSTILE_SITE_KEY,
    callback: function (token: string) {
      console.debug(`Challenge Success ${token}`);
      window.setTimeout(() => (modal.style.display = "none"), 1000);
      main(token);
    },
  });
};
