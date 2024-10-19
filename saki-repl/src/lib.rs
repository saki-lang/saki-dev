#![allow(clippy::missing_safety_doc)]

mod repl;
mod reporter;
mod theme;

use crate::repl::SakiRepl;
use crate::reporter::{PrintableError, RawError};
use lazy_static::lazy_static;
use std::ffi::c_char;
use std::slice;
use std::sync::Mutex;

const BUFFER_SIZE: usize = 2usize.pow(16);

lazy_static! {
    pub static ref SAKI_REPL: Mutex<SakiRepl> = Mutex::new(SakiRepl::new());
}

#[no_mangle]
pub extern "C" fn report_error(
    src: *const c_char,
    path: *const c_char,
    title: *const c_char,
    message: *const c_char,
    offset: i32, length: i32,
) {
    let error = RawError { src, path, title, message, offset, length };
    let err: miette::ErrReport = PrintableError::from(error).into();
    println!("{:?}", err);
}

#[no_mangle]
pub unsafe extern "C" fn repl_iterate(buffer: *mut std::os::raw::c_char) {
    assert!(!buffer.is_null());
    let output = SAKI_REPL.lock().unwrap().iterate();
    let output_bytes = output.as_bytes();
    if output_bytes.len() > BUFFER_SIZE {
        panic!("Output buffer too small");
    }
    // clear buffer
    for i in 0 .. BUFFER_SIZE {
        buffer.add(i).write_volatile(0);
    }
    let buf_slice = slice::from_raw_parts_mut(buffer as *mut u8, BUFFER_SIZE);
    buf_slice[..output_bytes.len()].copy_from_slice(output_bytes);
}

#[no_mangle]
pub extern "C" fn repl_create_buffer() -> *mut std::os::raw::c_char {
    let mut buffer = vec![0; BUFFER_SIZE];
    let ptr: *mut i32 = buffer.as_mut_ptr();
    std::mem::forget(buffer);
    ptr as *mut std::os::raw::c_char
}

#[no_mangle]
pub unsafe extern "C" fn repl_drop_buffer(buffer: *mut std::os::raw::c_char) {
    if !buffer.is_null() {
        unsafe { Vec::from_raw_parts(buffer as *mut u8, BUFFER_SIZE, BUFFER_SIZE); }
    }
}
