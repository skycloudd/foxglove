use chumsky::error::RichReason;
use chumsky::prelude::Rich;

use crate::typecheck::TypeInfo;
use crate::typed_ast::Type;
use crate::Span;

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    Typecheck(TypecheckError),
    ExpectedFound {
        expected: Vec<String>,
        found: Option<String>,
    },
    Custom(String),
    Many(Vec<Error>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypecheckError {
    UndefinedVariable {
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
        op: String,
        ty: Type,
    },
    CannotApplyBinaryOperator {
        span: Span,
        op: String,
        ty1: Type,
        ty2: Type,
    },
}

impl From<TypecheckError> for Error {
    fn from(err: TypecheckError) -> Self {
        Self::Typecheck(err)
    }
}

impl From<Rich<'_, String>> for Error {
    fn from(value: Rich<'_, String>) -> Self {
        fn convert_reason(reason: RichReason<String>) -> Error {
            match reason {
                RichReason::ExpectedFound { expected, found } => Error::ExpectedFound {
                    expected: expected.iter().map(ToString::to_string).collect(),
                    found: found.map(|s| s.to_string()),
                },
                RichReason::Custom(reason) => Error::Custom(reason),
                RichReason::Many(reasons) => {
                    Error::Many(reasons.into_iter().map(convert_reason).collect())
                }
            }
        }

        convert_reason(value.reason().clone())
    }
}
