/**
 * Use PBKDF2 to derive a coordinate from a password.
 */
export async function fromPassword(
  password: string,
): Promise<{ x: bigint; y: bigint }> {
  const salt = new Uint8Array([
    44, 76, 56, 90, 84, 50, 45, 64, 69, 24, 58, 23, 45, 4, 63, 58, 60, 24, 68,
    78, 46, 2, 40, 58,
  ]);
  const enc = new TextEncoder();
  const keym: CryptoKey = await window.crypto.subtle.importKey(
    "raw",
    enc.encode(password),
    "PBKDF2",
    false,
    ["deriveBits", "deriveKey"],
  );
  const key = await window.crypto.subtle.deriveKey(
    {
      name: "PBKDF2",
      salt,
      iterations: 100000,
      hash: "SHA-256",
    },
    keym,
    { name: "AES-GCM", length: 256 },
    true,
    ["encrypt", "decrypt"],
  );
  const exported = await window.crypto.subtle.exportKey("raw", key);
  const exportedKeyBuffer = new Uint8Array(exported);
  // 32 bytes
  return {
    x: bigIntFromBytes(exportedKeyBuffer.slice(0, 16)),
    y: bigIntFromBytes(exportedKeyBuffer.slice(16, 32)),
  };
}

// Convert a byte array to a bigint.
function bigIntFromBytes(arr: Uint8Array): bigint {
  let ret = 0n;
  for (let i = 0; i < arr.length; i++) {
    ret = (ret << 8n) + BigInt(arr[i]);
  }
  return ret;
}

// Convert a bigint to a hexadecimal string.
export function bigIntToHex(n: bigint): string {
  if (n < 0) {
    return "-0x" + (-n).toString(16);
  } else {
    return "0x" + n.toString(16);
  }
}

// Convert a hexadecimal string to a bigint.
export function bigIntFromHex(hexString: string): bigint {
  if (hexString[0] == "-") {
    return -BigInt(hexString.slice(1));
  } else {
    return BigInt(hexString);
  }
}

/**
 * Replaces the character at the specified index in a string with a new character.
 */
export function replaceAt(s: string, i: number, c: string): string {
  return s.substring(0, i) + c + s.substring(i + 1);
}

// Arithmetic on integers

type Int = number;

export function div(n: Int, m: Int): Int {
  return Math.floor(n / m);
}

export function mod(n: Int, m: Int): Int {
  return ((n % m) + m) % m;
}

export function divBigInt(n: bigint, m: bigint): bigint {
  if (n < 0n) {
    return n / m - (n % m === 0n ? 0n : 1n);
  } else {
    return n / m;
  }
}

export function modBigInt(n: bigint, m: bigint): bigint {
  return ((n % m) + m) % m;
}
