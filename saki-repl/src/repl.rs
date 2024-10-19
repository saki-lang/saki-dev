use std::process::exit;
use crate::theme::ParseSettings;
use reedline::{default_emacs_keybindings, ColumnarMenu, DefaultCompleter, DefaultPrompt, DefaultPromptSegment, Emacs, Highlighter, KeyCode, KeyModifiers, MenuBuilder, Reedline, ReedlineEvent, ReedlineMenu, Signal, StyledText};
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
            match self.line_editor.read_line(&prompt) {
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
