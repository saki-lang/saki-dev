
extern crate cbindgen;

use std::env;
use std::path::PathBuf;

pub fn main() {
    let crate_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    cbindgen::generate(crate_dir)
        .expect("Unable to generate bindings")
        .write_to_file(PathBuf::from("target/include/repl.h"));
}