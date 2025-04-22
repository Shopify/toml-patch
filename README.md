# `@shopify/toml-patch`

A WebAssembly-powered library to efficiently update TOML files, based on Rust's `toml_edit` crate.

[![CI](https://github.com/Shopify/toml-patch/actions/workflows/ci.yml/badge.svg)](https://github.com/Shopify/toml-patch/actions/workflows/ci.yml)

## Usage (JavaScript/TypeScript)

The primary function exported for JavaScript usage is `updateTomlValues`.

```javascript
import { updateTomlValues, patchSingleValue } from "@shopify/toml-patch";

const originalToml = `
[package]
name = "my-package"
version = "0.1.0"

[dependencies]
serde = "1.0"
`;

try {
  const updatedToml = updateTomlValues(
    originalToml,
    [
      patchSingleValue(['package', 'version'], '0.2.0'),
      patchSingleValue(['dependencies', 'serde'], '$undefined'),
      patchSingleValue(['new_table', 'key'], 'new value')
    ]
  );

  console.log(updatedToml);
  /*
  Output:

  [package]
  name = "my-package"
  version = "0.2.0"

  [dependencies]

  [new_table]
  key = "new value"
  */
} catch (error) {
  console.error("Failed to update TOML:", error);
}

```

**Arguments:**

1.  `toml_content` (string): The original TOML content.
2.  `paths_str` (string): A comma-separated string of dotted paths (e.g., `"table.key`).
3.  `values_str` (string): A comma-separated string of values corresponding to the paths. Values are parsed as TOML values (so strings need to be quoted). Use the special string `"$undefined"` to remove the specified key.

**Returns:**

*   (string): The updated TOML content as a string.

## Development

This project uses Rust and `wasm-pack`.

1.  **Install Rust:** [https://www.rust-lang.org/tools/install](https://www.rust-lang.org/tools/install)
2.  **Install `wasm-pack`:** `cargo install wasm-pack`
3.  **Build:** `wasm-pack build --target nodejs --release --scope="shopify"`
4.  **Test:** `wasm-pack test --node`
