use std::{error::Error, fmt, str::FromStr};

use toml_edit::{DocumentMut, Item, Table, TomlError as TomlEditError, Value, value};
use wasm_bindgen::prelude::*;

extern crate alloc;

#[cfg(target_arch = "wasm32")]
use lol_alloc::{FreeListAllocator, LockedAllocator};

#[cfg(target_arch = "wasm32")]
#[global_allocator]
static ALLOCATOR: LockedAllocator<FreeListAllocator> = LockedAllocator::new(FreeListAllocator::new());

/// Represents an error that occurred during TOML parsing or manipulation.
#[derive(Debug)]
pub struct TomlError {
  message: String,
}

impl fmt::Display for TomlError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "TOML Patch Error: {}", self.message)
  }
}

// Implement the standard Error trait for better integration with Rust error handling.
impl Error for TomlError {}

/// Converts a `toml_edit::TomlError` into our custom `TomlError`.
impl From<TomlEditError> for TomlError {
  fn from(error: TomlEditError) -> Self {
    TomlError { message: error.to_string() }
  }
}

/// Parses a TOML string using `toml_edit` and returns the serialized result.
/// Useful for validating and potentially normalizing TOML content.
/// Returns an error if the TOML content cannot be parsed.
#[wasm_bindgen(js_name = "echoToml")]
pub fn echo_toml(#[wasm_bindgen(js_name = "tomlContent")] toml_content: &str) -> Result<String, String> {
  DocumentMut::from_str(toml_content).map(|doc| doc.to_string()).map_err(|e| e.to_string())
}

/// Updates TOML content with the provided key-value pairs
///
/// # Arguments
/// * `toml_content` - A string containing valid TOML
/// * `patches` - An array of patches to apply; each patch is a tuple of a path and a value/values
///
/// An undefined value in a patch will remove the key from the TOML document.
///
/// # Returns
/// * `Ok(String)` - The updated TOML content as a string
/// * `Err(String)` - An error message if the operation fails
#[wasm_bindgen(js_name = "updateTomlValues")]
pub fn update_toml_values(
  #[wasm_bindgen(js_name = "tomlContent")] toml_content: &str,
  #[wasm_bindgen(
    unchecked_param_type = "[string[], number | string | boolean | undefined | (number | string | boolean)[]][]"
  )]
  patches: js_sys::Array,
) -> Result<String, String> {
  let mut doc = DocumentMut::from_str(toml_content).map_err(|e| format!("Failed to parse TOML: {}", e))?;

  let mut update_items = patches
    .iter()
    .enumerate()
    .map(|(index, tuple)| {
      let tuple_as_array = js_sys::Array::unchecked_from_js(tuple);
      let path = tuple_as_array.get(0);
      let path_as_array = js_sys::Array::unchecked_from_js(path);
      let path_as_vec = path_as_array.iter().map(|v| v.as_string().unwrap()).collect::<Vec<String>>();
      let value_or_values = tuple_as_array.get(1);
      let value_to_insert = parse_value_from_js_value(value_or_values);

      UpdateItem { original_index: index, path_parts: path_as_vec, value_to_insert }
    })
    .collect::<Vec<UpdateItem>>();

  sort_updates(&mut update_items);

  for item in update_items {
    apply_single_update_to_doc(
      &mut doc,
      &item.path_parts.iter().map(|s| s.as_str()).collect::<Vec<&str>>(),
      item.value_to_insert,
    )?;
  }

  Ok(doc.to_string())
}

/// This function handles the core logic of applying a single update item to the TOML document.
/// It navigates the document based on the `path_parts`, potentially creating tables implicitly,
/// and sets the final key to the `value_to_insert`.
/// It deals with various existing structures at the target path:
///   - If it finds a `Value`, it updates it directly.
///   - If it finds a `Table` or `ArrayOfTables`, it removes the existing item and proceeds
///     as if the path was initially empty, ensuring the final segment becomes a `Value`.
///   - If the path (or parts of it) doesn't exist, it creates the necessary `Table` structures.
///
/// # Arguments
/// * `doc` - A mutable reference to the `toml_edit::Document` to modify.
/// * `item` - The `UpdateItem` containing the path and value to apply (consumed).
///
/// # Returns
/// * `Ok(())` - If the update was applied successfully (either updating or creating).
/// * `Err(String)` - An error message if the update fails (e.g., path conflict).
fn apply_single_update_to_doc(
  doc: &mut DocumentMut,
  path_parts: &[&str],
  value_to_insert: Option<Value>,
) -> Result<(), String> {
  if path_parts.is_empty() {
    return Err("Cannot apply update with an empty path".to_string());
  }

  match value_to_insert {
    Some(value_to_insert) => {
      let direct_item = get_item_at_path_mut(doc, path_parts);
      match direct_item {
        Some(Item::Value(value)) => {
          *value = value_to_insert;
        }
        Some(Item::Table(_)) | Some(Item::ArrayOfTables(_)) => {
          // If the target path points to a Table or ArrayOfTables, but we need to insert a Value,
          // we must remove the existing structure first. We get the parent table and remove the item.
          // The loop will then reiterate, hitting the `None` case below to insert the value correctly.
          let mut parent_path = path_parts.to_vec();
          parent_path.pop(); // Safe because path_parts is not empty
          let parent_item = get_item_at_path_mut(doc, &parent_path);
          if let Some(parent_item) = parent_item.and_then(Item::as_table_like_mut) {
            parent_item.remove(path_parts[path_parts.len() - 1]);
          }
          // this will try again and should hit the None case below
          return apply_single_update_to_doc(doc, path_parts, Some(value_to_insert));
        }
        Some(_) => {}
        None => {
          // The path doesn't exist or was cleared in a previous iteration.
          // Create the necessary structure and insert the value.
          match path_parts {
            [] => {
              // This case should theoretically not be hit due to the initial check,
              // but handle it defensively.
            }
            [key] => {
              // Path has only one part, insert directly into the root table.
              simple_table_insertion(doc, &[], key, value_to_insert);
            }
            [parents @ .., key] => {
              // Path has multiple parts. Determine if we can use dotted keys
              // or need to create full nested tables.
              let parent_path_item = get_item_at_path_mut(doc, parents);
              match parent_path_item {
                None | Some(Item::None) | Some(Item::ArrayOfTables(_)) => {
                  // Parent path doesn't exist, is None, or is an ArrayOfTables.
                  // We generally need to create a new table structure.
                  match path_parts {
                    // Check grandparent to see if we can attach using a dotted key.
                    [_, key] => {
                      // No grandparent (parent is root)
                      simple_table_insertion(doc, parents, key, value_to_insert);
                    }
                    [grandparents @ .., parent, key] => {
                      let grandparent_path_item = get_item_at_path_mut(doc, grandparents);
                      match grandparent_path_item {
                        Some(Item::Table(grandparent_path_table)) => {
                          // Grandparent is a Table. We can insert `parent.key = value` as a dotted key table.
                          // This avoids creating a full `[parent]` table if possible.
                          let mut new_table = Table::new();
                          new_table.set_dotted(true);
                          new_table.insert(key, Item::Value(value_to_insert));

                          grandparent_path_table.insert(parent, Item::Table(new_table));
                        }
                        _ => {
                          // Grandparent is not a suitable table (or doesn't exist).
                          // Fall back to creating standard nested tables.
                          simple_table_insertion(doc, parents, key, value_to_insert);
                        }
                      }
                    }
                    _ => {
                      // Should be unreachable due to path_parts structure.
                      unreachable!("Invalid path structure encountered in apply_single_update_to_doc")
                    }
                  }
                }
                Some(Item::Value(_)) | Some(Item::Table(_)) => {
                  // Parent path exists and is a Value or Table.
                  // We can likely insert directly into the parent table structure.
                  // simple_table_insertion handles overwriting a Value with a Table if needed.
                  simple_table_insertion(doc, parents, key, value_to_insert);
                }
              }
            }
          }
        }
      }
      Ok(())
    }
    None => {
      delete_item_at_path(doc, path_parts);
      Ok(())
    }
  }
}

fn delete_item_at_path(doc: &mut DocumentMut, path_parts: &[&str]) {
  if path_parts.is_empty() {
    return;
  }

  let mut parent_parts = path_parts.to_vec();
  parent_parts.pop();
  let parent_item = get_item_at_path_mut(doc, &parent_parts);
  match parent_item {
    Some(Item::Table(table)) => {
      table.remove(path_parts[path_parts.len() - 1]);
    }
    _ => {
      // if the parent isn't there, or can't hold a child entry => neither is the child, and nothing to do.
    }
  }
}

fn parse_value_from_js_value(value_or_values: JsValue) -> Option<Value> {
  let value_item: Item;
  if value_or_values.is_array() {
    let values = js_sys::Array::from(&value_or_values);
    let with_nones = values.iter().filter_map(parse_value_from_js_value).collect::<Vec<Value>>();

    value_item = Item::Value(Value::from_iter(with_nones));
  } else if let Some(bool_value) = value_or_values.as_bool() {
    value_item = value(bool_value);
  } else if let Some(number_value) = value_or_values.as_f64() {
    if let Some(integer_value) = f64_to_i64_if_integer(number_value) {
      value_item = value(integer_value);
    } else {
      value_item = value(number_value);
    }
  } else if let Some(string_value) = value_or_values.as_string() {
    value_item = value(string_value);
  } else if value_or_values.is_undefined() {
    return None;
  } else {
    // unsupported type
    unreachable!("Unsupported type");
  }

  Some(
    value_item
      .as_value()
      .expect("Internal error: `value()` function should always produce an Item::Value variant")
      .to_owned(),
  )
}

fn f64_to_i64_if_integer(f: f64) -> Option<i64> {
  // 1. Check if it's a whole number (no fractional part).
  //    Using `trunc()` is often preferred over `fract()` for robustness
  //    against minor floating-point inaccuracies near integers.
  //    Also check if it's finite (not NaN or Infinity).
  if f.is_finite() && f == f.trunc() {
    // 2. Check if the value is within the representable range of i64.
    //    Casting the bounds to f64 is necessary for comparison.
    const I64_MIN_F64: f64 = i64::MIN as f64;
    const I64_MAX_F64: f64 = i64::MAX as f64;

    if (I64_MIN_F64..=I64_MAX_F64).contains(&f) {
      // 3. If both conditions are met, perform the cast.
      //    The `as` cast truncates, which is correct here since we know
      //    f == f.trunc().
      Some(f as i64)
    } else {
      // It's an integer but outside the i64 range.
      None
    }
  } else {
    // It's not a whole number or not finite.
    None
  }
}

/// Represents a single update operation derived from the input strings.
#[derive(Debug, Clone)]
struct UpdateItem {
  original_index: usize,          // Used for stable sorting if path lengths are equal
  path_parts: Vec<String>,        // The path split into components
  value_to_insert: Option<Value>, // The parsed TOML value to insert
}

/// Sorts update items: longest path first, then by original index for stability.
fn sort_updates(updates: &mut [UpdateItem]) {
  updates.sort_by(|a, b| {
    // Primary key: path length (reversed for longest first)
    b.path_parts.len().cmp(&a.path_parts.len())
        // Secondary key: original index (for stability)
        .then_with(|| a.original_index.cmp(&b.original_index))
  });
}

/// Recursively navigates the document structure using the provided path parts.
/// Returns a mutable reference `Some(&mut Item)` if the path leads to an existing item,
/// `None` otherwise. An empty `path_parts` slice refers to the root document item itself.
fn get_item_at_path_mut<'a>(doc: &'a mut DocumentMut, path_parts: &[&str]) -> Option<&'a mut Item> {
  let mut current_item: &mut Item = doc.as_item_mut();

  for part in path_parts {
    match current_item.as_table_like_mut() {
      Some(table_like) => {
        match table_like.get_mut(part) {
          Some(next_item) => current_item = next_item,
          None => return None, // Path segment not found
        }
      }
      None => return None, // Cannot traverse into a non-table-like item (e.g., value, array)
    }
  }
  Some(current_item)
}

/// Inserts a value into the document using a potentially nested path.
/// This function ensures that all necessary parent tables along the `parents` path exist,
/// creating them as implicit tables if they don't. Finally, it inserts the `key` with `to_set`
/// into the table identified by the `parents` path.
/// It handles overwriting existing *values* at intermediate paths by replacing them with tables.
fn simple_table_insertion(doc: &mut DocumentMut, parents: &[&str], key: &str, to_set: Value) {
  let mut current_table = doc.as_table_mut(); // Start from the root table

  for part in parents {
    // Ensure the current part exists and is a table.
    // If it exists but isn't a table (e.g., it's a value), it will be replaced.
    // If it doesn't exist, a new implicit table is created.
    if !current_table.contains_key(part) || !current_table[part].is_table() {
      let mut new_table = toml_edit::Table::new();
      new_table.set_implicit(true);
      current_table.insert(part, Item::Table(new_table));
    }
    // Now, get the mutable reference to the table for this part.
    // We expect this to succeed because we either found it or just created it.
    current_table =
      current_table.get_mut(part).and_then(Item::as_table_mut).expect("Should be a table after check/insert");
  }
  // After iterating through parents, 'current_table' is the direct parent for the final key.
  current_table.insert(key, Item::Value(to_set));
}

#[cfg(test)]
mod tests {
  use wasm_bindgen_test::*;

  use super::*;

  struct Patch<'a> {
    key_path: Vec<&'a str>,
    value: PatchValue<'a>,
  }

  enum PatchValue<'a> {
    String(&'a str),
    Float(f64),
    Integer(i64),
    Boolean(bool),
    Undefined,
    Array(Vec<PatchValue<'a>>),
  }

  fn patch_value_to_js_value(value: &PatchValue) -> JsValue {
    match value {
      PatchValue::String(s) => JsValue::from_str(s),
      PatchValue::Float(f) => JsValue::from_f64(*f),
      PatchValue::Integer(i) => JsValue::from_f64(*i as f64),
      PatchValue::Boolean(b) => JsValue::from_bool(*b),
      PatchValue::Undefined => JsValue::undefined(),
      PatchValue::Array(a) => {
        let value_items = js_sys::Array::new();
        for item in a {
          value_items.push(&patch_value_to_js_value(item));
        }
        JsValue::from(value_items)
      }
    }
  }

  fn test_update_toml_values(input: &str, patches: Vec<Patch>, expected: &str) {
    let patches_js = js_sys::Array::new();
    for patch in patches {
      let patch_js = js_sys::Array::new();
      let key_path_js = js_sys::Array::new();
      for key in &patch.key_path {
        key_path_js.push(&JsValue::from_str(key));
      }

      let patch_js_value = patch_value_to_js_value(&patch.value);

      patch_js.push(&key_path_js);
      patch_js.push(&patch_js_value);
      patches_js.push(&patch_js);
    }

    let result = update_toml_values(input, patches_js).expect("Failed to update TOML");
    let expected_doc = DocumentMut::from_str(expected).unwrap();
    let result_doc = DocumentMut::from_str(&result).unwrap();
    assert_eq!(expected_doc.to_string(), result_doc.to_string());
  }

  #[wasm_bindgen_test(unsupported = test)]
  fn test_echo_toml() {
    let input = r#"
[test]
key = "value"
"#;
    let output = echo_toml(input).expect("Failed to parse valid TOML");

    // TOML parser might reformat slightly, so use Document for comparison
    let parsed_input = DocumentMut::from_str(input).unwrap();
    let parsed_output = DocumentMut::from_str(&output).unwrap();
    assert_eq!(parsed_input.to_string(), parsed_output.to_string());
  }

  #[wasm_bindgen_test(unsupported = test)]
  fn test_echo_toml_invalid() {
    let input = "invalid toml";
    let result = echo_toml(input);
    assert!(result.is_err());
  }

  #[wasm_bindgen_test(unsupported = test)]
  fn test_empty_file_single_item() {
    test_update_toml_values(r#""#, vec![Patch { key_path: vec!["a"], value: PatchValue::Integer(1) }], r#"a = 1"#);
  }

  #[wasm_bindgen_test(unsupported = test)]
  fn test_empty_file_double_item() {
    test_update_toml_values(
      r#""#,
      vec![Patch { key_path: vec!["a", "b"], value: PatchValue::Integer(1) }],
      r#"[a]
b = 1
"#,
    );
  }

  #[wasm_bindgen_test(unsupported = test)]
  fn test_uses_single_dot_if_space_for_it() {
    test_update_toml_values(
      r#"
[a]
b = 1
"#,
      vec![Patch { key_path: vec!["a", "c", "d"], value: PatchValue::Integer(2) }],
      r#"
[a]
b = 1
c.d = 2
"#,
    );
  }

  #[wasm_bindgen_test(unsupported = test)]
  fn test_does_not_use_dots_if_no_home_for_it() {
    test_update_toml_values(
      r#"
[a]
b = 1
"#,
      vec![Patch { key_path: vec!["a", "c", "d", "e"], value: PatchValue::Integer(2) }],
      r#"
[a]
b = 1

[a.c.d]
e = 2
"#,
    );
  }

  #[wasm_bindgen_test(unsupported = test)]
  fn test_update_toml_basic_updates_existing() {
    test_update_toml_values(
      r#"
[section]
key = "old_value"
other = 123
"#,
      vec![Patch { key_path: vec!["section", "key"], value: PatchValue::String("new_value") }],
      r#"
[section]
key = "new_value"
other = 123
"#,
    );
  }

  #[wasm_bindgen_test(unsupported = test)]
  fn test_update_toml_add_new() {
    test_update_toml_values(
      r#"
[section]
existing = true
"#,
      vec![Patch { key_path: vec!["section", "new_key"], value: PatchValue::Integer(42) }],
      r#"
[section]
existing = true
new_key = 42
"#,
    );
  }

  #[wasm_bindgen_test(unsupported = test)]
  fn test_update_toml_nested() {
    test_update_toml_values(
      r#"
[parent]
"#,
      vec![Patch { key_path: vec!["parent", "child", "grandchild"], value: PatchValue::Boolean(true) }],
      r#"
[parent]
child.grandchild = true
"#,
    );
  }

  #[wasm_bindgen_test(unsupported = test)]
  fn test_update_toml_multiple() {
    test_update_toml_values(
      r#"
[section1]
key1 = "value1"

[section2]
key2 = 42
"#,
      vec![
        Patch { key_path: vec!["section1", "key1"], value: PatchValue::String("updated") },
        Patch { key_path: vec!["section2", "key2"], value: PatchValue::Integer(99) },
        Patch { key_path: vec!["section3", "new"], value: PatchValue::Float(3.25) },
      ],
      r#"
[section1]
key1 = "updated"

[section2]
key2 = 99

[section3]
new = 3.25
"#,
    );
  }

  #[wasm_bindgen_test(unsupported = test)]
  fn test_update_toml_types() {
    test_update_toml_values(
      r#"
[test]
"#,
      vec![
        Patch { key_path: vec!["test", "string"], value: PatchValue::String("hello") },
        Patch { key_path: vec!["test", "int"], value: PatchValue::Integer(123) },
        Patch { key_path: vec!["test", "float"], value: PatchValue::Float(45.67) },
        Patch { key_path: vec!["test", "bool"], value: PatchValue::Boolean(true) },
      ],
      r#"
[test]
string = "hello"
int = 123
float = 45.67
bool = true
"#,
    );
  }

  #[wasm_bindgen_test(unsupported = test)]
  fn test_adds_new_entry_to_existing_table() {
    test_update_toml_values(
      r#"
[section]
foo = 1
"#,
      vec![Patch { key_path: vec!["section", "new_key"], value: PatchValue::Integer(42) }],
      r#"
[section]
foo = 1
new_key = 42
"#,
    );
  }

  #[wasm_bindgen_test(unsupported = test)]
  fn test_creates_new_table_if_needed_and_likes_dots() {
    test_update_toml_values(
      r#"
[section]
foo = 1

[keep.this]
thing = true
"#,
      vec![
        Patch { key_path: vec!["section", "subsection", "new_key"], value: PatchValue::Integer(42) },
        Patch { key_path: vec!["section", "subsection", "something", "else", "here"], value: PatchValue::Integer(43) },
      ],
      r#"
[section]
foo = 1

[section.subsection]
new_key = 42

[section.subsection.something.else]
here = 43

[keep.this]
thing = true
"#,
    );
  }

  #[wasm_bindgen_test(unsupported = test)]
  fn test_creates_minimal_new_tables() {
    test_update_toml_values(
      r#"
[foo]
existing = 1
"#,
      vec![
        Patch { key_path: vec!["foo", "bar", "aaa"], value: PatchValue::Integer(1) },
        Patch { key_path: vec!["foo", "bar", "bbb"], value: PatchValue::Integer(2) },
        Patch { key_path: vec!["foo", "bar", "ccc", "ddd"], value: PatchValue::Integer(3) },
      ],
      r#"
[foo]
existing = 1

[foo.bar]
aaa = 1
bbb = 2

[foo.bar.ccc]
ddd = 3
"#,
    );
  }

  #[wasm_bindgen_test(unsupported = test)]
  fn test_replaces_table_if_setting_a_value() {
    test_update_toml_values(
      r#"
[a.b.c]
d = 1
"#,
      vec![Patch { key_path: vec!["a", "b"], value: PatchValue::Integer(1) }],
      r#"[a]
b = 1
"#,
    );
  }

  #[wasm_bindgen_test(unsupported = test)]
  fn test_replaces_table_if_setting_a_value_with_no_parent() {
    test_update_toml_values(
      r#"
[a]
d = 1
"#,
      vec![Patch { key_path: vec!["a"], value: PatchValue::Integer(1) }],
      r#"a = 1"#,
    );
  }

  #[wasm_bindgen_test(unsupported = test)]
  fn test_replaces_array_of_table_if_setting_a_value() {
    test_update_toml_values(
      r#"
[[a.b.c]]
d = 1
"#,
      vec![Patch { key_path: vec!["a", "b", "c"], value: PatchValue::Integer(1) }],
      r#"[a.b]
c = 1
"#,
    );
  }

  #[wasm_bindgen_test(unsupported = test)]
  fn test_inserts_into_document_root_if_new() {
    test_update_toml_values(
      r#"
[a]
b = 1
"#,
      vec![Patch { key_path: vec!["c"], value: PatchValue::Integer(1) }],
      r#"c = 1

[a]
b = 1
"#,
    );
  }

  #[wasm_bindgen_test(unsupported = test)]
  fn test_replaces_if_parent_is_currently_a_value() {
    test_update_toml_values(
      r#"
[a]
b = 1
"#,
      vec![Patch { key_path: vec!["a", "b", "c"], value: PatchValue::Integer(1) }],
      r#"
[a]

[a.b]
c = 1
"#,
    );
  }

  #[wasm_bindgen_test(unsupported = test)]
  fn test_adds_as_dotted_if_grandparent_is_a_table() {
    test_update_toml_values(
      r#"
[a]
d = 1
"#,
      vec![Patch { key_path: vec!["a", "b", "c"], value: PatchValue::Integer(1) }],
      r#"
[a]
d = 1
b.c = 1
"#,
    );
  }

  #[wasm_bindgen_test(unsupported = test)]
  fn test_attaches_to_the_right_thing() {
    test_update_toml_values(
      r#"
[a.b.c]
existing = 1
"#,
      vec![Patch { key_path: vec!["a", "b", "c", "new"], value: PatchValue::Integer(1) }],
      r#"
[a.b.c]
existing = 1
new = 1
"#,
    );
  }

  #[wasm_bindgen_test(unsupported = test)]
  fn test_does_minimal_updates_attaching_to_existing_tables() {
    test_update_toml_values(
      r#"
[foo.a.b.c.d.e]
existing = 1
"#,
      vec![
        Patch { key_path: vec!["foo", "a", "b", "c", "d", "e", "f", "g"], value: PatchValue::Integer(1) },
        Patch { key_path: vec!["foo", "a", "b", "c", "d", "e", "f", "h"], value: PatchValue::Integer(2) },
        Patch { key_path: vec!["foo", "a", "b", "c", "d", "e", "f", "i", "j"], value: PatchValue::Integer(3) },
      ],
      r#"
[foo.a.b.c.d.e]
existing = 1

[foo.a.b.c.d.e.f]
g = 1
h = 2

[foo.a.b.c.d.e.f.i]
j = 3
"#,
    );
  }

  #[wasm_bindgen_test(unsupported = test)]
  fn test_removes_value_if_setting_to_undefined() {
    test_update_toml_values(
      r#"
      [section]
      key = "value"
      "#,
      vec![Patch { key_path: vec!["section", "key"], value: PatchValue::Undefined }],
      r#"
      [section]
      "#,
    );
  }

  #[wasm_bindgen_test(unsupported = test)]
  fn test_removes_value_from_document_if_setting_to_undefined() {
    test_update_toml_values(
      r#"
      key = "value"
      "#,
      vec![Patch { key_path: vec!["key"], value: PatchValue::Undefined }],
      r#"      "#,
    );
  }

  #[wasm_bindgen_test(unsupported = test)]
  fn test_does_nothing_if_removing_something_that_does_not_exist() {
    test_update_toml_values(
      r#"
      [section]
      "#,
      vec![Patch { key_path: vec!["key"], value: PatchValue::Undefined }],
      r#"
      [section]
      "#,
    );
  }

  /// Test that comments are preserved when updating TOML files.
  ///
  /// Note on `toml_edit` comment preservation behavior:
  /// - Comments attached to structures (tables, arrays) and sections are generally preserved.
  /// - Comments attached to key-value pairs:
  ///   - Leading comments (immediately above the key) are preserved.
  ///   - Inline comments (on the same line, after the value) are typically *lost* when the value is replaced.
  ///   - Comments on untouched key-value pairs remain untouched.
  /// - Blank lines and other formatting might be adjusted by `toml_edit` during parsing and serialization.
  #[wasm_bindgen_test(unsupported = test)]
  fn test_preserves_comments() {
    test_update_toml_values(
      r#"
# This is a top comment
[section] # Comment after section header
# Comment before key
key = "value" # Comment after value - this comment will be lost when key is updated
untouched = 123 # This comment will be preserved

# Comment before another section
[another] # Another section comment
foo = 42 # Number comment
"#,
      vec![
        Patch { key_path: vec!["section", "key"], value: PatchValue::String("updated") },
        Patch { key_path: vec!["another", "bar"], value: PatchValue::Boolean(true) },
      ],
      r#"
# This is a top comment
[section] # Comment after section header
# Comment before key
key = "updated"
untouched = 123 # This comment will be preserved

# Comment before another section
[another] # Another section comment
foo = 42 # Number comment
bar = true
"#,
    );
  }

  #[wasm_bindgen_test(unsupported = test)]
  fn test_patch_an_array_value() {
    test_update_toml_values(
      r#"
      [a]
      b = [1, 2, 3]
      "#,
      vec![Patch {
        key_path: vec!["a", "b"],
        value: PatchValue::Array(vec![PatchValue::Integer(4), PatchValue::Integer(5), PatchValue::String("hello")]),
      }],
      r#"
      [a]
      b = [4, 5, "hello"]
      "#,
    );
  }

  #[wasm_bindgen_test(unsupported = test)]
  fn test_sort_updates_empty() {
    let mut updates: Vec<UpdateItem> = vec![];
    sort_updates(&mut updates);
    assert!(updates.is_empty());
  }

  #[wasm_bindgen_test(unsupported = test)]
  fn test_sort_updates_single_item() {
    let mut updates = vec![UpdateItem {
      original_index: 0,
      path_parts: vec!["a".to_string(), "b".to_string()],
      value_to_insert: Some(value(1).as_value().unwrap().to_owned()),
    }];
    sort_updates(&mut updates);
    assert_eq!(updates.len(), 1);
    assert_eq!(updates[0].path_parts, vec!["a", "b"]);
  }
}
