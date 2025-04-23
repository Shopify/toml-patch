import { describe, it, expect } from "vitest";
import {
  updateTomlValues,
  patchSingleValue,
  patchArrayValues,
} from "../pkg/toml_patch.js";

const sampleToml = `
# This is a sample TOML file
title = "TOML Example"

[owner]
name = "Test User"
organization = "Test Org"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ] # Comment after array
enabled = true
`;

describe("updateTomlValues", () => {
  it("should return an empty string for empty input", () => {
    expect(updateTomlValues("", [])).toBe("");
  });

  it("updating TOML makes minimal changes and preserves as much as possible", async () => {
    const output = updateTomlValues(sampleToml, [
      patchSingleValue(["owner", "dotted", "notation"], "123.5"),
      patchSingleValue(["database", "server"], "changed"),
      patchSingleValue(["top_level"], "true"),
      patchSingleValue(["owner", "organization"], "$undefined"),
      patchArrayValues(["database", "backup_ports"], ["8003", "8004"]),
    ]);

    const expected = `
# This is a sample TOML file
title = "TOML Example"
top_level = true

[owner]
name = "Test User"
dotted.notation = 123.5

[database]
server = "changed"
ports = [ 8001, 8001, 8002 ] # Comment after array
enabled = true
backup_ports = [8003, 8004]
`;

    expect(output).toBe(expected);
  });
});
