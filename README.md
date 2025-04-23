# `@shopify/toml-patch`

A WebAssembly-powered library to efficiently update TOML files, based on Rust's `toml_edit` crate.

[![CI](https://github.com/Shopify/toml-patch/actions/workflows/ci.yml/badge.svg)](https://github.com/Shopify/toml-patch/actions/workflows/ci.yml)

## Usage (JavaScript/TypeScript)

The primary function exported for JavaScript usage is `updateTomlValues`. Take a look at the `example` folder.

```javascript
import { updateTomlValues } from "@shopify/toml-patch";

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
      [['package', 'version'], '0.2.0'],
      [['dependencies', 'serde'], undefined],
      [['new_table', 'key'], 'new value'],
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

## Development

This project uses Rust and `wasm-pack`.

1.  **Install Rust:** [https://www.rust-lang.org/tools/install](https://www.rust-lang.org/tools/install)
2.  **Install `wasm-pack`:** `cargo install wasm-pack`
3.  **Build:** `wasm-pack build --target nodejs --release --scope="shopify"`
4.  **Test:** `wasm-pack test --node`
