use std::borrow::Cow;
use std::process::exit;
use crate::theme::ParseSettings;
use reedline::{default_emacs_keybindings, ColumnarMenu, DefaultCompleter, DefaultPrompt, DefaultPromptSegment, Emacs, Highlighter, KeyCode, KeyModifiers, MenuBuilder, Prompt, PromptEditMode, PromptHistorySearch, Reedline, ReedlineEvent, ReedlineMenu, Signal, StyledText, ValidationResult, Validator};
use syntect::easy::HighlightLines;
use syntect::highlighting::{Style, Theme};
use syntect::parsing::{SyntaxDefinition, SyntaxSet, SyntaxSetBuilder};

use nu_ansi_term::Color as NuColor;
use nu_ansi_term::Style as NuStyle;

const SYNTAX_YAML: &str = include_str!("../saki.sublime-syntax.yml");
const THEME_JSON: &str = include_str!("../theme.json");

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

pub struct SakiHighlighter {
    pub syntax: SyntaxSet,
    pub theme: Theme,
}

impl SakiHighlighter {
    pub fn new() -> Self {
        let syntax_definition = SyntaxDefinition::load_from_str(SYNTAX_YAML, true, None)
            .expect("Failed to load syntax definition");
        let theme_settings = serde_json::from_str(THEME_JSON)
            .expect("Failed to parse theme settings");
        let theme = Theme::parse_settings(theme_settings)
            .expect("Failed to load theme settings");
        let mut syntax_set = SyntaxSetBuilder::new();
        syntax_set.add(syntax_definition);
        SakiHighlighter { syntax: syntax_set.build(), theme }
    }
}

impl Highlighter for SakiHighlighter {
    fn highlight(&self, line: &str, _cursor: usize) -> StyledText {
        let syntax = self.syntax.find_syntax_by_name("Saki").unwrap();
        let mut highlighter = HighlightLines::new(syntax, &self.theme);
        // Highlight the line using syntect's mechanism
        let ranges: Vec<(Style, &str)> = highlighter
            .highlight_line(line, &self.syntax)
            .expect("Failed to highlight line");
        let ranges: Vec<(NuStyle, String)> = ranges.into_iter().map(|(style, piece)| {
            let fg = style.foreground;
            let style = NuStyle::new().fg(NuColor::Rgb(fg.r, fg.g, fg.b));
            (style, piece.to_string())
        }).collect();
        StyledText { buffer: ranges }
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
        self.0.render_prompt_indicator(prompt_mode)
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
        let mut in_char = false;
        let mut in_comment = false;
        let mut block_comment_depth = 0;

        let mut parens = 0;
        let mut braces = 0;
        let mut brackets = 0;

        let mut chars = line.chars().peekable();
        let mut prev_char = None; // Track previous character for escape sequence handling

        while let Some(c) = chars.next() {
            // Handle single-line comments: ignore everything after `//`
            if !in_string && !in_char && !in_comment && c == '/' {
                if chars.peek() == Some(&'/') {
                    break;  // Ignore rest of the line in a single-line comment
                } else if chars.peek() == Some(&'*') {
                    in_comment = true;
                    block_comment_depth += 1;
                    chars.next(); // Skip the '*'
                    continue;
                }
            }

            // Handle block comments: keep track of depth
            if in_comment {
                if c == '*' && chars.peek() == Some(&'/') {
                    chars.next();
                    block_comment_depth -= 1;
                    if block_comment_depth == 0 {
                        in_comment = false;
                    }
                    continue;
                } else if c == '/' && chars.peek() == Some(&'*') {
                    chars.next();
                    block_comment_depth += 1;
                    continue;
                }
                continue;  // Ignore content inside block comment
            }

            // Handle string literals (including escaped quotes)
            if !in_char && !in_comment {
                if c == '"' {
                    if in_string {
                        // Handle escaped quotes inside string
                        if prev_char != Some('\\') {
                            in_string = false;
                        }
                    } else {
                        in_string = true;
                    }
                    prev_char = Some(c);
                    continue;
                }

                // Handle character literals (e.g., `'a'`)
                if c == '\'' {
                    in_char = !in_char;
                    prev_char = Some(c);
                    continue;
                }
            }

            // If inside a string literal or char literal, ignore all structural characters
            if in_string || in_char {
                prev_char = Some(c);
                continue;
            }

            // Track parentheses, braces, and brackets
            match c {
                '(' => parens += 1,
                ')' => parens -= 1,
                '{' => braces += 1,
                '}' => braces -= 1,
                '[' => brackets += 1,
                ']' => brackets -= 1,
                _ => {}
            }

            // If at any point we have more closing than opening, we know the line is incomplete
            if parens < 0 || braces < 0 || brackets < 0 {
                return ValidationResult::Incomplete;
            }

            prev_char = Some(c);
        }

        // A line is complete if:
        // - No unbalanced parentheses, braces, or brackets
        // - We are not inside a string literal or block comment
        if parens == 0 && braces == 0 && brackets == 0 && !in_string && !in_comment {
            ValidationResult::Complete
        } else {
            ValidationResult::Incomplete
        }
    }
}

#[cfg(test)]
mod tests {

    use reedline::{ValidationResult, Validator};
    use crate::repl::SakiValidator;

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
        assert!(is_line_complete("val a = 1"));
        // Single-line braces and parentheses
        assert!(is_line_complete("val a = (1 + 2)"));
        assert!(is_line_complete("val a = { 1 + 2 }"));
        // Complete string literal with braces inside
        assert!(is_line_complete("val a = \"{\""));
        // Braces and parentheses closed correctly
        assert!(is_line_complete("val a = [{()}]"));
    }

    #[test]
    fn test_incomplete_line() {
        // Open brace without closing
        assert!(!is_line_complete("def foo(a: Int): Int = {"));
        // Open parentheses without closing
        assert!(!is_line_complete("val a = ("));
        // Unmatched quotes
        assert!(!is_line_complete("val a = \"{"));
        // Mixed braces and parentheses, unbalanced
        assert!(!is_line_complete("val a = [({})"));
    }

    #[test]
    fn test_comment_handling() {
        // Comment containing a brace should be ignored
        assert!(is_line_complete("val a = 1 // {"));
        // Block comment should be ignored
        assert!(is_line_complete("val a = /* { */ 1"));
        // Open block comment that doesn't close
        assert!(!is_line_complete("val a = /*"));
        // Properly closed block comment
        assert!(is_line_complete("val a = /* { */ 1 /* } */"));
    }

    #[test]
    fn test_string_handling() {
        // String containing braces should be ignored
        assert!(is_line_complete("val a = \"{(}\""));
        // Open string without closing
        assert!(!is_line_complete("val a = \"{"));
        // Empty string should be complete
        assert!(is_line_complete("val a = \"\""));
        // Escaped quotes inside a string
        assert!(is_line_complete("val a = \"\\\"{\""));
        // Unmatched quote at the end
        assert!(!is_line_complete("val a = \"\"{\""));
    }

    #[test]
    fn test_complex_cases() {
        // Mixed braces and comments
        assert!(is_line_complete("val a = { 1 + /* comment */ 2 }"));
        // Single-line comment at the end
        assert!(is_line_complete("val a = 1 // comment {"));
        // Block comment that closes properly, despite containing braces
        assert!(is_line_complete("val a = /* { */ { /* } */ }"));
        // String and open brace outside of string
        assert!(!is_line_complete("val a = \"}\"{"));
        // String with escaped quotes and braces
        assert!(is_line_complete("val a = \"\\\"}\""));
    }
}


