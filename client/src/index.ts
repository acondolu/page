import { Page } from "./Page";
import * as Config from "./Config";

let p: Page | undefined;

async function main(token: string) {
  console.debug("Challenge Success");
  if (p) {
    // Page already exists, just renew the token.
    p.remote.renewToken(token);
  } else {
    // Create the page for the first time.
    p = new Page(token, Config.contentEditable);
    await p.init();
  }
}

(window as any).onloadTurnstileCallback = function () {
  const modal = document.getElementById("captcha") as HTMLElement;
  (window as any).turnstile.render("#cf-turnstile", {
    sitekey: Config.CF_TURNSTILE_SITE_KEY,
    callback: (token: string) => {
      modal.style.display = "none";
      main(token);
    },
    'expired-callback': () => {
      modal.style.display = "block";
      (window as any).turnstile.reset();
    },
    'refresh-expired': 'auto',
  });
};
