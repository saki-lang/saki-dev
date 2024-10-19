#![allow(clippy::missing_safety_doc)]

mod repl;
mod reporter;
mod theme;

use crate::repl::SakiRepl;
use crate::reporter::{PrintableError, RawError};
use lazy_static::lazy_static;
use std::ffi::c_char;
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
    let output = std::ffi::CString::new(SAKI_REPL.lock().unwrap().iterate()).unwrap();
    unsafe {
        std::ptr::copy_nonoverlapping(output.as_ptr(), buffer, BUFFER_SIZE);
    }
}

#[no_mangle]
pub extern "C" fn repl_create_buffer() -> *mut std::os::raw::c_char {
    let buffer = vec![0; BUFFER_SIZE];
    std::ffi::CString::new(buffer).unwrap().into_raw()
}

#[no_mangle]
pub unsafe extern "C" fn repl_drop_buffer(buffer: *mut std::os::raw::c_char) {
    if buffer.is_null() { return; }
    let _ = std::ffi::CString::from_raw(buffer);
}
