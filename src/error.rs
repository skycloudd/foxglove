use crate::typecheck::TypeInfo;
use crate::{Span, Spanned};
use ariadne::{Color, Fmt, Span as _};
use chumsky::error::SimpleReason;
use chumsky::prelude::Simple;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Report {
    msg: String,
    code: Code,
    errors: Vec<ActualError>,
    help_message: Option<String>,
    note: Option<String>,
}

impl Report {
    fn new(
        msg: String,
        code: Code,
        errors: Vec<ActualError>,
        help_message: Option<String>,
        note: Option<String>,
    ) -> Self {
        Self {
            msg,
            code,
            errors,
            help_message,
            note,
        }
    }

    pub fn custom(span: Span, msg: &str) -> Self {
        Self::new(
            msg.to_owned(),
            ErrorKind::Custom.into(),
            vec![ActualError::new(span, msg.to_owned())],
            None,
            None,
        )
    }

    fn unexpected(span: Span, found: Option<&str>, expected: Vec<Option<&str>>) -> Self {
        Self::new(
            format!(
                "{}, expected {}",
                if found.is_some() {
                    "Unexpected token in input"
                } else {
                    "Unexpected end of input"
                },
                if expected.is_empty() {
                    "something else".to_owned()
                } else {
                    expected
                        .iter()
                        .map(|expected| match expected {
                            Some(expected) => expected.fg(Color::Yellow).to_string(),
                            None => "end of input".fg(Color::Yellow).to_string(),
                        })
                        .collect::<Vec<_>>()
                        .join(", ")
                }
            ),
            ErrorKind::Unexpected.into(),
            vec![ActualError::new(
                span,
                format!(
                    "Unexpected token `{}`",
                    found.unwrap_or("end of file").fg(Color::Yellow)
                ),
            )],
            None,
            None,
        )
    }

    fn unclosed(span: Span, delimiter: &str, found: Option<&str>) -> Self {
        Self::new(
            format!("Unclosed delimiter `{}`", delimiter.fg(Color::Yellow)),
            ErrorKind::Unclosed.into(),
            vec![
                ActualError::new(
                    span.clone(),
                    format!("Unclosed delimiter `{}`", delimiter.fg(Color::Yellow)),
                ),
                ActualError::new(
                    span,
                    format!(
                        "Must be closed before this `{}`",
                        found.unwrap_or("end of file").fg(Color::Yellow)
                    ),
                ),
            ],
            None,
            None,
        )
    }

    pub fn cannot_infer_type(span: Span) -> Self {
        Self::new(
            "Cannot infer type".to_owned(),
            ErrorKind::CannotInferType.into(),
            vec![ActualError::new(span, "Cannot infer type".to_owned())],
            None,
            None,
        )
    }

    pub fn type_conflict(one: Spanned<TypeInfo>, two: Spanned<TypeInfo>) -> Self {
        Self::new(
            "Type conflict".to_owned(),
            ErrorKind::TypeConflict.into(),
            vec![
                ActualError::new(one.1, format!("This type `{}`", one.0)),
                ActualError::new(two.1, format!("This type `{}`", two.0)),
            ],
            None,
            None,
        )
    }

    pub fn msg(&self) -> &str {
        &self.msg
    }

    pub fn code(&self) -> &Code {
        &self.code
    }

    pub fn errors(&self) -> &[ActualError] {
        &self.errors
    }

    pub fn help_message(&self) -> Option<&str> {
        self.help_message.as_deref()
    }

    pub fn note(&self) -> Option<&str> {
        self.note.as_deref()
    }

    pub fn offset(&self) -> usize {
        self.errors
            .iter()
            .map(|e| e.span().start())
            .min()
            .unwrap_or(0)
    }
}

impl From<Simple<String, Span>> for Report {
    fn from(e: Simple<String, Span>) -> Self {
        match e.reason() {
            SimpleReason::Custom(msg) => Self::custom(e.span(), msg),
            SimpleReason::Unexpected => Self::unexpected(
                e.span(),
                e.found().map(String::as_str),
                e.expected()
                    .map(|e| e.as_ref().map(String::as_str))
                    .collect(),
            ),
            SimpleReason::Unclosed { span, delimiter } => {
                Self::unclosed(span.clone(), delimiter, e.found().map(String::as_str))
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ActualError {
    span: Span,
    msg: String,
}

impl ActualError {
    fn new(span: Span, msg: String) -> Self {
        Self { span, msg }
    }

    pub fn span(&self) -> Span {
        self.span.clone()
    }

    pub fn msg(&self) -> &str {
        &self.msg
    }
}

enum ErrorKind {
    Custom,
    Unexpected,
    Unclosed,
    CannotInferType,
    TypeConflict,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Code(pub String);

impl From<ErrorKind> for Code {
    fn from(kind: ErrorKind) -> Self {
        Code(format!(
            "E{:04}",
            match kind {
                ErrorKind::Custom => 1,
                ErrorKind::Unexpected => 2,
                ErrorKind::Unclosed => 3,
                ErrorKind::CannotInferType => 4,
                ErrorKind::TypeConflict => 5,
            }
        ))
    }
}
