use crate::highlighter::SakiHighlighter;
use miette::highlighters::SyntectHighlighter;
use miette::{Diagnostic, NamedSource, SourceOffset, SourceSpan};
use std::ffi::{c_char, CStr};
use thiserror::Error;

pub fn init_reporter() {
    miette::set_hook(Box::new(|_| {
        let highlighter: SyntectHighlighter = SakiHighlighter::new().into();
        let handler = miette::MietteHandlerOpts::new()
            .terminal_links(true)
            .context_lines(5)
            .with_syntax_highlighting(highlighter)
            .build();
        Box::new(handler)
    })).expect("Failed to set up miette error handler");
}

#[derive(Error, Debug, Diagnostic)]
#[error("{title}")]
pub struct PrintableError {
    #[source_code]
    src: NamedSource<String>,
    title: String,
    #[label("{message}")]
    span: SourceSpan,
    message: String,
}

#[repr(C)]
pub struct RawError {
    pub src: *const c_char,
    pub path: *const c_char,
    pub title: *const c_char,
    pub message: *const c_char,
    pub offset: i32,
    pub length: i32,
}

impl From<RawError> for PrintableError {
    fn from(err: RawError) -> Self {
        let src = unsafe { CStr::from_ptr(err.src).to_string_lossy().into_owned() };
        let path = unsafe { CStr::from_ptr(err.path).to_string_lossy().into_owned() };
        let byte_offset = char_to_byte_offset(&src, err.offset as usize).unwrap_or(0);
        Self {
            src: NamedSource::new(path, src),
            title: unsafe { CStr::from_ptr(err.title).to_string_lossy().into_owned() },
            span: SourceSpan::new(SourceOffset::from(byte_offset), err.length as usize),
            message: unsafe { CStr::from_ptr(err.message).to_string_lossy().into_owned() },
        }
    }
}

fn char_to_byte_offset(string: &str, char_offset: usize) -> Option<usize> {
    string.char_indices().nth(char_offset).map(|(byte_offset, _)| byte_offset)
}
