import { bench, describe } from "vitest";
import {
  updateTomlValues,
  patchSingleValue,
  patchArrayValues,
  updateTomlValues2,
} from "../pkg/toml_patch.js";
import { sampleToml } from "./test.test.js";

describe("TOML update", () => {
  bench("with patch structures", () => {
    updateTomlValues(sampleToml, [
      patchSingleValue(["owner", "dotted", "notation"], "123.5"),
      patchSingleValue(["database", "server"], "changed"),
      patchSingleValue(["top_level"], "true"),
      patchSingleValue(["owner", "organization"], "$undefined"),
      patchArrayValues(["database", "backup_ports"], ["8003", "8004"]),
    ]);
  });

  bench("with JS values", () => {
    updateTomlValues2(sampleToml, [
      [["owner", "dotted", "notation"], 123.5],
      [["database", "server"], "changed"],
      [["top_level"], true],
      [["owner", "organization"], undefined],
      [
        ["database", "backup_ports"],
        [8003, 8004],
      ],
    ]);
  });
});
