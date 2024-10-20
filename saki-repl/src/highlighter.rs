use crate::theme::ParseSettings;
use miette::highlighters::SyntectHighlighter;
use nu_ansi_term::Color as NuColor;
use nu_ansi_term::Style as NuStyle;
use reedline::StyledText;
use syntect::easy::HighlightLines;
use syntect::highlighting::{Style, Theme};
use syntect::parsing::{SyntaxDefinition, SyntaxSet, SyntaxSetBuilder};

const SYNTAX_YAML: &str = include_str!("../saki.sublime-syntax.yml");
const THEME_JSON: &str = include_str!("../theme.json");

pub struct SakiHighlighter {
    pub syntax: SyntaxSet,
    pub theme: Theme,
}

impl Default for SakiHighlighter {
    fn default() -> Self {
        Self::new()
    }
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

impl From<SakiHighlighter> for SyntectHighlighter {
    fn from(highlighter: SakiHighlighter) -> Self {
        SyntectHighlighter::new(highlighter.syntax, highlighter.theme, false)
    }
}

impl reedline::Highlighter for SakiHighlighter {
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
