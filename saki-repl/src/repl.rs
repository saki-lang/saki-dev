use reedline::{default_emacs_keybindings, ColumnarMenu, DefaultCompleter, DefaultPrompt, DefaultPromptSegment, Emacs, KeyCode, KeyModifiers, MenuBuilder, Prompt, PromptEditMode, PromptHistorySearch, PromptViMode, Reedline, ReedlineEvent, ReedlineMenu, Signal, ValidationResult, Validator};
use std::borrow::Cow;
use std::process::exit;
use crate::highlighter::SakiHighlighter;

pub struct SakiRepl {
    line_editor: Reedline,
    counter: usize,
}

impl Default for SakiRepl {
    fn default() -> Self { Self::new() }
}

impl SakiRepl {
    pub fn new() -> Self {
        let keywords: Vec<String> = vec![
            "def", "type", "impl", "instance", "record", "inductive", "let",
            "flat", "prefix", "postfix", "self", "operator", "binary", "unary",
            "left-assoc", "right-assoc", "tighter-than", "looser-than", "same-as",
            "if", "then", "else", "match", "case", "forall", "exists", "eval",
        ].into_iter().map(String::from).collect();

        let completer = Box::new(DefaultCompleter::new_with_wordlen(keywords.clone(), 2));
        let completion_menu = Box::new(ColumnarMenu::default().with_name("completion_menu"));

        let mut keybindings = default_emacs_keybindings();
        keybindings.add_binding(
            KeyModifiers::NONE,
            KeyCode::Tab,
            ReedlineEvent::UntilFound(vec![
                ReedlineEvent::Menu("completion_menu".to_string()),
                ReedlineEvent::MenuNext,
            ]),
        );

        let edit_mode = Box::new(Emacs::new(keybindings));

        let line_editor = Reedline::create()
            .with_highlighter(Box::new(SakiHighlighter::new()))
            .with_validator(Box::new(SakiValidator))
            .with_completer(completer)
            .with_menu(ReedlineMenu::EngineCompleter(completion_menu))
            .with_edit_mode(edit_mode);

        SakiRepl { line_editor, counter: 0 }
    }

    pub fn iterate(&mut self) -> String {
        loop {
            let prompt = DefaultPrompt {
                left_prompt: DefaultPromptSegment::Basic(" saki ".into()),
                right_prompt: DefaultPromptSegment::Basic(format!(" -<{}>- ", self.counter)),
            };
            match self.line_editor.read_line(&SakiPrompt(prompt)) {
                Ok(Signal::Success(buffer)) => {
                    if !buffer.trim().is_empty() {
                        if buffer.trim() == "exit" {
                            exit(0);
                        }
                        self.counter += 1;
                        return buffer;
                    }
                }
                Ok(Signal::CtrlD) => exit(0),
                Ok(Signal::CtrlC) | Err(_) => continue,
            }
        }
    }
}

pub struct SakiPrompt(DefaultPrompt);

impl Prompt for SakiPrompt {
    fn render_prompt_left(&self) -> Cow<str> {
        self.0.render_prompt_left()
    }

    fn render_prompt_right(&self) -> Cow<str> {
        self.0.render_prompt_right()
    }

    fn render_prompt_indicator(&self, prompt_mode: PromptEditMode) -> Cow<str> {
        match prompt_mode {
            PromptEditMode::Default | PromptEditMode::Emacs => "> ".into(),
            PromptEditMode::Vi(vi_mode) => match vi_mode {
                PromptViMode::Normal => "> ".into(),
                PromptViMode::Insert => ": ".into(),
            },
            PromptEditMode::Custom(str) => format!("({str})").into(),
        }
    }

    fn render_prompt_multiline_indicator(&self) -> Cow<str> {
        let prompt_length = match &self.0.left_prompt {
            DefaultPromptSegment::Basic(prompt) => prompt.len(),
            _ => 4
        };
        Cow::Owned(" ".repeat(prompt_length) + "| ")
    }

    fn render_prompt_history_search_indicator(&self, history_search: PromptHistorySearch) -> Cow<str> {
        self.0.render_prompt_history_search_indicator(history_search)
    }
}

pub struct SakiValidator;

impl Validator for SakiValidator {
    fn validate(&self, line: &str) -> ValidationResult {
        let mut in_string = false;
        let mut in_single_line_comment = false;
        let mut in_block_comment = false;

        let mut parens = 0;
        let mut braces = 0;
        let mut brackets = 0;

        let mut chars = line.chars().peekable();
        let mut prev_char = None; // Track previous character for escape sequence handling

        while let Some(current) = chars.next() {
            // Handle single-line comments: ignore everything after `//` until the end of the line
            if !in_string && !in_block_comment && !in_single_line_comment && current == '/' {
                if chars.peek() == Some(&'/') {
                    in_single_line_comment = true;
                    chars.next(); // Skip the second '/'
                    continue;
                } else if chars.peek() == Some(&'*') {
                    in_block_comment = true;
                    chars.next(); // Skip the '*'
                    continue;
                }
            }

            // End single-line comment at the end of the line
            if in_single_line_comment {
                if current == '\n' {
                    in_single_line_comment = false;
                }
                continue;  // Ignore the rest of the line in single-line comment
            }

            // Handle block comments: end the block comment when encountering '*/'
            if in_block_comment {
                if current == '*' && chars.peek() == Some(&'/') {
                    chars.next(); // Skip the '/'
                    in_block_comment = false;
                }
                continue;  // Ignore content inside block comment
            }

            // Handle string literals (including escaped quotes)
            if !in_block_comment && current == '"' {
                if in_string {
                    // Handle escaped quotes inside string
                    if prev_char != Some('\\') {
                        in_string = false;
                    }
                } else {
                    in_string = true;
                }
                prev_char = Some(current);
                continue;
            }

            // If inside a string literal, ignore all structural characters
            if in_string {
                prev_char = Some(current);
                continue;
            }

            // Track parentheses, braces, and brackets
            match current {
                '(' => parens += 1,
                ')' => parens -= 1,
                '{' => braces += 1,
                '}' => braces -= 1,
                '[' => brackets += 1,
                ']' => brackets -= 1,
                _ => {}
            }

            // If at any point we have more closing than opening, the line is incomplete
            if parens < 0 || braces < 0 || brackets < 0 {
                return ValidationResult::Incomplete;
            }

            prev_char = Some(current);
        }

        // A line is complete if:
        // - No unbalanced parentheses, braces, or brackets
        // - We are not inside a string literal, block comment, or single-line comment
        if parens == 0 && braces == 0 && brackets == 0 && !in_string && !in_block_comment {
            ValidationResult::Complete
        } else {
            ValidationResult::Incomplete
        }
    }


}

#[cfg(test)]
mod tests {
    use crate::repl::SakiValidator;
    use reedline::{ValidationResult, Validator};

    fn is_line_complete(line: &str) -> bool {
        let validator = SakiValidator;
        match validator.validate(line) {
            ValidationResult::Complete => true,
            ValidationResult::Incomplete => false,
        }
    }

    #[test]
    fn test_complete_line() {
        // Simple assignment, no unbalanced brackets
        assert!(is_line_complete("let a = 1"));
        // Single-line braces and parentheses
        assert!(is_line_complete("let a = (1 + 2)"));
        assert!(is_line_complete("let a = { 1 + 2 }"));
        // Complete string literal with braces inside
        assert!(is_line_complete("let a = \"{\""));
        // Braces and parentheses closed correctly
        assert!(is_line_complete("let a = [{()}]"));
    }

    #[test]
    fn test_incomplete_line() {
        // Open brace without closing
        assert!(!is_line_complete("def foo(a: Int): Int = {"));
        // Open parentheses without closing
        assert!(!is_line_complete("let a = ("));
        // Unmatched quotes
        assert!(!is_line_complete("let a = \"{"));
        // Mixed braces and parentheses, unbalanced
        assert!(!is_line_complete("let a = [({})"));
    }

    #[test]
    fn test_comment_handling() {
        // Comment containing a brace should be ignored
        assert!(is_line_complete("let a = 1 // {"));
        // Block comment should be ignored
        assert!(is_line_complete("let a = /* { */ 1"));
        // Open block comment that doesn't close
        assert!(!is_line_complete("let a = /*"));
        // Properly closed block comment
        assert!(is_line_complete("let a = /* { */ 1 /* } */"));
        // Nested block comments
        assert!(is_line_complete("let a = /* /* 1 */"));
        // Block comment with closing delimiter inside a comment
        assert!(is_line_complete("let a = /* 1 */ */"));
        // Single-line comment at the end
        assert!(is_line_complete("let a = { // comment \n 1 }"));
    }

    #[test]
    fn test_block_comment_handling() {
        // Valid block comment
        assert!(is_line_complete("let a = /* /* */ 1"));
        assert!(is_line_complete("let a = /* */ 1 */"));
        assert!(is_line_complete("let a = /* /* */ */"));
        assert!(is_line_complete("let a = /* 1 */ */"));
    }

    #[test]
    fn test_string_handling() {
        // String containing braces should be ignored
        assert!(is_line_complete("let a = \"{(}\""));
        // Open string without closing
        assert!(!is_line_complete("let a = \"{"));
        // Empty string should be complete
        assert!(is_line_complete("let a = \"\""));
        // Escaped quotes inside a string
        assert!(is_line_complete("let a = \"\\\"{\""));
        // Unmatched quote at the end
        assert!(!is_line_complete("let a = \"\"{\""));
    }

    #[test]
    fn test_complex_cases() {
        // Mixed braces and comments
        assert!(is_line_complete("let a = { 1 + /* comment */ 2 }"));
        // Single-line comment at the end
        assert!(is_line_complete("let a = 1 // comment {"));
        // Block comment that closes properly, despite containing braces
        assert!(is_line_complete("let a = /* { */ { /* } */ }"));
        // String and open brace outside of string
        assert!(!is_line_complete("let a = \"}\"{"));
        // String with escaped quotes and braces
        assert!(is_line_complete("let a = \"\\\"}\""));
    }
}


