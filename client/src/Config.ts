const env = process.env.NODE_ENV || "development";

const isProduction = env == "production";

export const CF_TURNSTILE_SITE_KEY = isProduction
  ? "0x4AAAAAAAzDIfRnxqCcAMqi"
  : "1x00000000000000000000AA";

export function idempotencyKey(): string {
  return isProduction
    ? crypto.randomUUID()
    : "00000000-00000000-00000000-00000000";
}

export function wsUrl(): string {
  return isProduction ? "wss://page.acondolu.me" : "ws://localhost:8765";
}

export const contentEditable: boolean = true;
