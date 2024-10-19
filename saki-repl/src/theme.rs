use std::str::FromStr;
use serde_json::Value;
use syntect::highlighting::{
    Color, FontStyle, ParseThemeError, ScopeSelectors, 
    StyleModifier, Theme, ThemeItem, ThemeSettings, UnderlineOption
};
use syntect::highlighting::ParseThemeError::{*};

pub trait ParseSettings: Sized {
    type Error;
    fn parse_settings(settings: Value) -> Result<Self, Self::Error>;
}

impl ParseSettings for Theme {
    type Error = ParseThemeError;

    fn parse_settings(settings: Value) -> Result<Theme, Self::Error> {
        let mut obj = match settings {
            Value::Object(obj) => obj,
            _ => return Err(IncorrectSyntax),
        };
        let name = match obj.remove("name") {
            Some(Value::String(name)) => Some(name),
            None => None,
            _ => return Err(IncorrectSyntax),
        };
        let author = match obj.remove("author") {
            Some(Value::String(author)) => Some(author),
            None => None,
            _ => return Err(IncorrectSyntax),
        };
        let items = match obj.remove("settings") {
            Some(Value::Array(items)) => items,
            _ => return Err(IncorrectSyntax),
        };
        let mut iter = items.into_iter();
        let mut settings = match iter.next() {
            Some(Value::Object(mut obj)) => {
                match obj.remove("settings") {
                    Some(settings) => ThemeSettings::parse_settings(settings)?,
                    None => return Err(UndefinedSettings),
                }
            }
            _ => return Err(UndefinedSettings),
        };
        if let Some(Value::Object(obj)) = obj.remove("gutterSettings") {
            for (key, value) in obj {
                let color = Color::parse_settings(value).ok();
                match &key[..] {
                    "background" => {
                        settings.gutter = settings.gutter.or(color)
                    }
                    "foreground" => {
                        settings.gutter_foreground = settings.gutter_foreground.or(color)
                    }
                    _ => (),
                }
            }
        }
        let mut scopes = Vec::new();
        for json in iter {
            if let Ok(item) = ThemeItem::parse_settings(json) {
                scopes.push(item);
            }
        }
        Ok(Theme { name, author, settings, scopes })
    }
}

impl ParseSettings for Color {
    type Error = ParseThemeError;

    fn parse_settings(settings: Value) -> Result<Color, Self::Error> {
        match settings {
            Value::String(value) => Color::from_str(&value),
            _ => Err(IncorrectColor),
        }
    }
}

impl ParseSettings for StyleModifier {
    type Error = ParseThemeError;

    fn parse_settings(settings: Value) -> Result<StyleModifier, Self::Error> {
        let mut obj = match settings {
            Value::Object(obj) => obj,
            _ => return Err(ColorShemeScopeIsNotObject),
        };
        let font_style = match obj.remove("fontStyle") {
            Some(Value::String(value)) => Some(FontStyle::from_str(&value)?),
            None => None,
            Some(c) => return Err(IncorrectFontStyle(c.to_string())),
        };
        let foreground = match obj.remove("foreground") {
            Some(Value::String(value)) => Some(Color::from_str(&value)?),
            None => None,
            _ => return Err(IncorrectColor),
        };
        let background = match obj.remove("background") {
            Some(Value::String(value)) => Some(Color::from_str(&value)?),
            None => None,
            _ => return Err(IncorrectColor),
        };

        Ok(StyleModifier { foreground, background, font_style })
    }
}

impl ParseSettings for ThemeItem {
    type Error = ParseThemeError;

    fn parse_settings(settings: Value) -> Result<ThemeItem, Self::Error> {
        let mut obj = match settings {
            Value::Object(obj) => obj,
            _ => return Err(ColorShemeScopeIsNotObject),
        };
        let scope = match obj.remove("scope") {
            Some(Value::String(value)) => ScopeSelectors::from_str(&value)?,
            _ => return Err(ScopeSelectorIsNotString(format!("{:?}", obj))),
        };
        let style = match obj.remove("settings") {
            Some(settings) => StyleModifier::parse_settings(settings)?,
            None => return Err(IncorrectSettings),
        };
        Ok(ThemeItem { scope, style })
    }
}

impl ParseSettings for ThemeSettings {
    type Error = ParseThemeError;

    fn parse_settings(json: Value) -> Result<ThemeSettings, Self::Error> {
        let mut settings = ThemeSettings::default();

        let obj = match json {
            Value::Object(obj) => obj,
            _ => return Err(ColorShemeSettingsIsNotObject),
        };

        for (key, value) in obj {
            match &key[..] {
                "foreground" => settings.foreground = Color::parse_settings(value).ok(),
                "background" => settings.background = Color::parse_settings(value).ok(),
                "caret" => settings.caret = Color::parse_settings(value).ok(),
                "lineHighlight" => settings.line_highlight = Color::parse_settings(value).ok(),
                "misspelling" => settings.misspelling = Color::parse_settings(value).ok(),
                "minimapBorder" => settings.minimap_border = Color::parse_settings(value).ok(),
                "accent" => settings.accent = Color::parse_settings(value).ok(),

                "popupCss" => settings.popup_css = value.as_str().map(|s| s.to_owned()),
                "phantomCss" => settings.phantom_css = value.as_str().map(|s| s.to_owned()),

                "bracketContentsForeground" => {
                    settings.bracket_contents_foreground = Color::parse_settings(value).ok()
                }
                "bracketContentsOptions" => {
                    settings.bracket_contents_options = UnderlineOption::parse_settings(value).ok()
                }
                "bracketsForeground" => {
                    settings.brackets_foreground = Color::parse_settings(value).ok()
                }
                "bracketsBackground" => {
                    settings.brackets_background = Color::parse_settings(value).ok()
                }
                "bracketsOptions" => {
                    settings.brackets_options = UnderlineOption::parse_settings(value).ok()
                }
                "tagsForeground" => settings.tags_foreground = Color::parse_settings(value).ok(),
                "tagsOptions" => {
                    settings.tags_options = UnderlineOption::parse_settings(value).ok()
                }
                "highlight" => settings.highlight = Color::parse_settings(value).ok(),
                "findHighlight" => settings.find_highlight = Color::parse_settings(value).ok(),
                "findHighlightForeground" => {
                    settings.find_highlight_foreground = Color::parse_settings(value).ok()
                }
                "gutter" => settings.gutter = Color::parse_settings(value).ok(),
                "gutterForeground" => {
                    settings.gutter_foreground = Color::parse_settings(value).ok()
                }
                "selection" => settings.selection = Color::parse_settings(value).ok(),
                "selectionForeground" => {
                    settings.selection_foreground = Color::parse_settings(value).ok()
                }
                "selectionBorder" => settings.selection_border = Color::parse_settings(value).ok(),
                "inactiveSelection" => {
                    settings.inactive_selection = Color::parse_settings(value).ok()
                }
                "inactiveSelectionForeground" => {
                    settings.inactive_selection_foreground = Color::parse_settings(value).ok()
                }
                "guide" => settings.guide = Color::parse_settings(value).ok(),
                "activeGuide" => settings.active_guide = Color::parse_settings(value).ok(),
                "stackGuide" => settings.stack_guide = Color::parse_settings(value).ok(),
                "shadow" => settings.shadow = Color::parse_settings(value).ok(),
                _ => (), // E.g. "shadowWidth" and "invisibles" are ignored
            }
        }
        Ok(settings)
    }
}

impl ParseSettings for UnderlineOption {
    type Error = ParseThemeError;

    fn parse_settings(settings: Value) -> Result<UnderlineOption, Self::Error> {
        match settings {
            Value::String(value) => UnderlineOption::from_str(&value),
            _ => Err(IncorrectUnderlineOption),
        }
    }
}
