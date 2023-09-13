use crate::typecheck::TypeInfo;
use crate::typed_ast::{BinOp, PrefixOp, Type};
use crate::{Span, Spanned};
use ariadne::{Color, Fmt};
use chumsky::error::RichReason;
use chumsky::prelude::Rich;

#[derive(Clone, PartialEq)]
pub enum Error {
    Typecheck(TypecheckError),
    ExpectedFound {
        span: Span,
        expected: Vec<String>,
        found: Option<String>,
    },
    Custom(Span, String),
    Many(Vec<Error>),
}

type Message = String;
type Spans = Vec<Spanned<(String, Color)>>;
type Note = Option<String>;

impl Error {
    pub fn make_report(&self) -> Vec<(Message, Spans, Note)> {
        match self {
            Error::Typecheck(e) => vec![e.make_report()],
            Error::ExpectedFound {
                span,
                expected,
                found,
            } => vec![(
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
                            .map(|expected| expected.fg(Color::Yellow).to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    }
                ),
                vec![(
                    (
                        format!(
                            "Unexpected token {}",
                            format!("'{}'", found.as_ref().unwrap_or(&"eof".to_string()))
                                .fg(Color::Yellow)
                        ),
                        Color::Yellow,
                    ),
                    *span,
                )],
                None,
            )],
            Error::Custom(span, msg) => {
                vec![(
                    msg.to_string(),
                    vec![((String::new(), Color::Yellow), *span)],
                    None,
                )]
            }
            Error::Many(errors) => errors.iter().flat_map(Error::make_report).collect(),
        }
    }

    pub fn code(&self) -> u32 {
        match self {
            Error::Typecheck(e) => match e {
                TypecheckError::UndefinedVariable { .. } => 2,
                TypecheckError::UndefinedFunction { .. } => 3,
                TypecheckError::CannotInferType { .. } => 4,
                TypecheckError::TypeMismatch { .. } => 5,
                TypecheckError::CannotApplyUnaryOperator { .. } => 6,
                TypecheckError::CannotApplyBinaryOperator { .. } => 7,
                TypecheckError::IncorrectNumberOfArguments { .. } => 8,
                TypecheckError::MainFunctionHasParameters { .. } => 9,
                TypecheckError::MainFunctionHasWrongReturnType { .. } => 10,
                TypecheckError::MissingMainFunction(_) => 11,
                TypecheckError::ContinueOutsideOfLoop(_) => 12,
                TypecheckError::BreakOutsideOfLoop(_) => 13,
            },
            Error::ExpectedFound { .. } => 1,
            Error::Custom(_, _) => 0,
            Error::Many(errs) => errs.iter().map(Error::code).max().unwrap_or(0),
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum TypecheckError {
    UndefinedVariable {
        name: String,
        span: Span,
    },
    UndefinedFunction {
        name: String,
        span: Span,
    },
    CannotInferType {
        span: Span,
    },
    TypeMismatch {
        span1: Span,
        span2: Span,
        ty1: TypeInfo,
        ty2: TypeInfo,
    },
    CannotApplyUnaryOperator {
        span: Span,
        op: PrefixOp,
        ty: Type,
    },
    CannotApplyBinaryOperator {
        span: Span,
        op: BinOp,
        ty1: Type,
        ty2: Type,
    },
    IncorrectNumberOfArguments {
        span: Span,
        expected: usize,
        found: usize,
    },
    MainFunctionHasParameters {
        span: Span,
    },
    MainFunctionHasWrongReturnType {
        span: Span,
        expected: Type,
        found: Type,
    },
    MissingMainFunction(Span),
    ContinueOutsideOfLoop(Span),
    BreakOutsideOfLoop(Span),
}

impl TypecheckError {
    fn make_report(&self) -> (Message, Spans, Note) {
        match self {
            TypecheckError::UndefinedVariable { name, span } => (
                format!("Undefined variable '{}'", name.fg(Color::Yellow)),
                vec![(
                    ("not found in this scope".to_string(), Color::Yellow),
                    *span,
                )],
                None,
            ),
            TypecheckError::UndefinedFunction { name, span } => (
                format!("Undefined function '{}'", name.fg(Color::Yellow)),
                vec![(
                    ("not found in this scope".to_string(), Color::Yellow),
                    *span,
                )],
                None,
            ),
            TypecheckError::CannotInferType { span } => (
                "Cannot infer type".to_string(),
                vec![(
                    (
                        "Cannot infer the type of this expression".to_string(),
                        Color::Yellow,
                    ),
                    *span,
                )],
                Some("help: try adding a type annotation".to_string()),
            ),
            TypecheckError::TypeMismatch {
                span1,
                span2,
                ty1,
                ty2,
            } => (
                "Type mismatch".to_string(),
                vec![
                    ((format!("Type '{:?}' here", ty1), Color::Yellow), *span1),
                    ((format!("Type '{:?}' here", ty2), Color::Yellow), *span2),
                ],
                None,
            ),
            TypecheckError::CannotApplyUnaryOperator { span, op, ty } => (
                format!(
                    "Cannot apply operator '{}' to type '{}'",
                    op.fg(Color::Yellow),
                    format!("{:?}", ty).fg(Color::Yellow)
                ),
                vec![(
                    (
                        format!(
                            "Cannot apply this operator to type '{}'",
                            format!("{:?}", ty).fg(Color::Yellow)
                        ),
                        Color::Yellow,
                    ),
                    *span,
                )],
                None,
            ),
            TypecheckError::CannotApplyBinaryOperator { span, op, ty1, ty2 } => (
                format!(
                    "Cannot apply binary operator '{}' to types '{}' and '{}'",
                    op.fg(Color::Yellow),
                    format!("{:?}", ty1).fg(Color::Yellow),
                    format!("{:?}", ty2).fg(Color::Yellow)
                ),
                vec![(
                    (
                        format!(
                            "Cannot apply this operator to types '{}' and '{}'",
                            format!("{:?}", ty1).fg(Color::Yellow),
                            format!("{:?}", ty2).fg(Color::Yellow)
                        ),
                        Color::Yellow,
                    ),
                    *span,
                )],
                None,
            ),
            TypecheckError::IncorrectNumberOfArguments {
                span,
                expected,
                found,
            } => (
                format!(
                    "Incorrect number of arguments, expected {}, found {}",
                    expected.fg(Color::Yellow),
                    found.fg(Color::Yellow)
                ),
                vec![(
                    (
                        format!(
                            "Incorrect number of arguments, expected {}, found {}",
                            expected.fg(Color::Yellow),
                            found.fg(Color::Yellow)
                        ),
                        Color::Yellow,
                    ),
                    *span,
                )],
                None,
            ),
            TypecheckError::MainFunctionHasParameters { span } => (
                "Main function cannot have parameters".to_string(),
                vec![(
                    (
                        "Main function cannot have parameters".to_string(),
                        Color::Yellow,
                    ),
                    *span,
                )],
                None,
            ),
            TypecheckError::MainFunctionHasWrongReturnType {
                span,
                expected,
                found,
            } => (
                format!(
                    "Main function has wrong return type, expected '{}', found '{}'",
                    format!("{:?}", expected).fg(Color::Yellow),
                    format!("{:?}", found).fg(Color::Yellow)
                ),
                vec![(
                    (
                        format!(
                            "Main function has wrong return type, expected '{}', found '{}'",
                            format!("{:?}", expected).fg(Color::Yellow),
                            format!("{:?}", found).fg(Color::Yellow)
                        ),
                        Color::Yellow,
                    ),
                    *span,
                )],
                None,
            ),
            TypecheckError::MissingMainFunction(span) => (
                "Missing main function".to_string(),
                vec![(("Missing main function".to_string(), Color::Yellow), *span)],
                None,
            ),
            TypecheckError::ContinueOutsideOfLoop(span) => (
                "Continue statement outside of a loop".to_string(),
                vec![(
                    ("Continue outside of loop".to_string(), Color::Yellow),
                    *span,
                )],
                None,
            ),
            TypecheckError::BreakOutsideOfLoop(span) => (
                "Break statement outside of a loop".to_string(),
                vec![(("Break outside of loop".to_string(), Color::Yellow), *span)],
                None,
            ),
        }
    }
}

impl From<TypecheckError> for Error {
    fn from(err: TypecheckError) -> Self {
        Self::Typecheck(err)
    }
}

impl From<Rich<'_, String>> for Error {
    fn from(value: Rich<'_, String>) -> Self {
        fn convert_reason(reason: RichReason<String>, span: Span) -> Error {
            match reason {
                RichReason::ExpectedFound { expected, found } => Error::ExpectedFound {
                    span,
                    expected: expected.iter().map(ToString::to_string).collect(),
                    found: found.map(|s| s.to_string()),
                },
                RichReason::Custom(reason) => Error::Custom(span, reason),
                RichReason::Many(reasons) => Error::Many(
                    reasons
                        .into_iter()
                        .map(|reason| convert_reason(reason, span))
                        .collect(),
                ),
            }
        }

        convert_reason(value.reason().clone(), *value.span())
    }
}
