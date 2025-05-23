import { describe, it, expect } from "vitest";
import { updateTomlValues, getTomlLocations } from "../pkg/toml_patch.js";

export const sampleToml = `
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

export const expected = `
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

describe("updateTomlValues", () => {
  it("should return an empty string for empty input", () => {
    expect(updateTomlValues("", [])).toBe("");
  });

  it("updating TOML makes minimal changes and preserves as much as possible", () => {
    const output = updateTomlValues(sampleToml, [
      [["owner", "dotted", "notation"], 123.5],
      [["database", "server"], "changed"],
      [["top_level"], true],
      [["owner", "organization"], undefined],
      [
        ["database", "backup_ports"],
        [8003, 8004],
      ],
    ]);

    expect(output).toBe(expected);
  });
});

const fileForLocations = `
top = 3

[a]
b = 1
c = [true, false]

[[b]]
yes = 1

[[b]]
no = 2

[c.d.e.f.g]
h = { "hello" = { "world" = 1 } }
`;

describe("getTomlLocations", () => {
  it("should return the correct locations for a sample TOML file", () => {
    const locations = getTomlLocations(fileForLocations);
    expect(locations).toMatchInlineSnapshot(`
      [
        {
          "end": 8,
          "path": [],
          "start": 0,
        },
        {
          "end": 8,
          "path": [
            "top",
          ],
          "start": 7,
        },
        {
          "end": 37,
          "path": [
            "a",
          ],
          "start": 10,
        },
        {
          "end": 19,
          "path": [
            "a",
            "b",
          ],
          "start": 18,
        },
        {
          "end": 37,
          "path": [
            "a",
            "c",
          ],
          "start": 24,
        },
        {
          "end": 29,
          "path": [
            "a",
            "c",
            "0",
          ],
          "start": 25,
        },
        {
          "end": 36,
          "path": [
            "a",
            "c",
            "1",
          ],
          "start": 31,
        },
        {
          "end": 52,
          "path": [
            "b",
            "0",
          ],
          "start": 39,
        },
        {
          "end": 52,
          "path": [
            "b",
            "0",
            "yes",
          ],
          "start": 51,
        },
        {
          "end": 66,
          "path": [
            "b",
            "1",
          ],
          "start": 54,
        },
        {
          "end": 66,
          "path": [
            "b",
            "1",
            "no",
          ],
          "start": 65,
        },
        {
          "end": 113,
          "path": [
            "c",
            "d",
            "e",
            "f",
            "g",
          ],
          "start": 68,
        },
        {
          "end": 113,
          "path": [
            "c",
            "d",
            "e",
            "f",
            "g",
            "h",
          ],
          "start": 84,
        },
        {
          "end": 111,
          "path": [
            "c",
            "d",
            "e",
            "f",
            "g",
            "h",
            "hello",
          ],
          "start": 96,
        },
        {
          "end": 109,
          "path": [
            "c",
            "d",
            "e",
            "f",
            "g",
            "h",
            "hello",
            "world",
          ],
          "start": 108,
        },
      ]
    `);

    const found = locations.find(
      (loc) => loc.path.join(".") === "c.d.e.f.g.h.hello"
    )!;
    expect(
      fileForLocations.slice(found.start, found.end)
    ).toMatchInlineSnapshot(`"{ "world" = 1 }"`);
  });
});
