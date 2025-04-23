import { bench, describe } from "vitest";
import { updateTomlValues } from "../pkg/toml_patch.js";
import { sampleToml } from "./test.test.js";
import { parse, stringify } from "@iarna/toml";
import { merge } from "lodash-es";
import { expect } from "vitest";

const inputs = [
  [["owner", "dotted", "notation"], 123.5],
  [["database", "server"], "changed"],
  [["top_level"], true],
  [["owner", "organization"], undefined],
  [
    ["database", "backup_ports"],
    [8003, 8004],
  ],
] as any;

function createPatchFromKeyPath(
  keys: string[],
  value: unknown
): { [key: string]: unknown } {
  if (keys.length === 1) {
    return { [keys[0]]: value };
  }

  const obj: { [key: string]: unknown } = {};
  let currentObj = obj;

  for (let i = 0; i < keys.length - 1; i++) {
    const key = keys[i];
    if (key) {
      currentObj[key] = {};
      currentObj = currentObj[key] as { [key: string]: unknown };
    }
  }

  const lastKey = keys[keys.length - 1];
  if (lastKey) {
    currentObj[lastKey] = value;
  }

  return obj;
}

describe("TOML update", () => {
  bench("JS", () => {
    const toml = parse(sampleToml);

    const toMerge = inputs.reduce((acc, [keyPath, value]) => {
      const valuePatch = createPatchFromKeyPath(keyPath, value);
      return merge(acc, valuePatch);
    }, {});

    const merged = merge(toml, toMerge);

    stringify(merged);
  });

  bench("WASM", () => {
    updateTomlValues(sampleToml, inputs);
  });
});
