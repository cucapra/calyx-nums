//! Interface to the Sollya tool.

use std::ffi::OsStr;
use std::io::{self, Write};
use std::process::{Command, Stdio};
use std::string::FromUtf8Error;
use std::{fmt, thread};

use crate::fpcore::ast;
use crate::utils::mangling::Mangle;

/// Invokes Sollya with the given command.
pub fn sollya<S>(cmd: &[u8], args: &[S]) -> Result<String, SollyaError>
where
    S: AsRef<OsStr>,
{
    let mut child = Command::new("sollya")
        .arg("--flush")
        .arg("--warnonstderr")
        .arg("--args")
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .spawn()?;

    let mut stdin = child.stdin.take().unwrap();

    let output = thread::scope(|scope| {
        scope.spawn(move || {
            stdin.write_all(cmd).unwrap();
        });

        child.wait_with_output()
    })?;

    let status = output.status;

    if !status.success() {
        return Err(SollyaError::Status(
            status
                .code()
                .map_or(ExitStatus::Unknown, ExitStatus::from_code),
        ));
    }

    String::from_utf8(output.stdout).map_err(SollyaError::from)
}

/// Exit status from a Sollya invocation.
#[derive(Clone, Copy, Debug)]
pub enum ExitStatus {
    Success = 0,
    InternalError = 1,
    LanguageError = 2,
    NoQuit = 3,
    IncompleteInput = 4,
    Unknown,
}

impl ExitStatus {
    fn from_code(code: i32) -> ExitStatus {
        match code {
            0 => ExitStatus::Success,
            1 => ExitStatus::InternalError,
            2 => ExitStatus::LanguageError,
            3 => ExitStatus::NoQuit,
            4 => ExitStatus::IncompleteInput,
            _ => ExitStatus::Unknown,
        }
    }
}

impl fmt::Display for ExitStatus {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let description = match self {
            ExitStatus::Success => "success",
            ExitStatus::InternalError => "internal error",
            ExitStatus::LanguageError => "language error",
            ExitStatus::NoQuit => "reached EOF without quit",
            ExitStatus::IncompleteInput => "reached EOF with incomplete input",
            ExitStatus::Unknown => "unknown status",
        };

        write!(f, "{}", description)
    }
}

/// An error resulting from a Sollya invocation.
#[derive(Debug)]
#[non_exhaustive]
pub enum SollyaError {
    Io(io::Error),
    Utf8,
    Status(ExitStatus),
}

impl fmt::Display for SollyaError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SollyaError::Io(err) => write!(f, "{}", err),
            SollyaError::Utf8 => {
                write!(f, "invocation returned invalid UTF-8")
            }
            SollyaError::Status(status) => {
                write!(f, "invocation failed ({})", status)
            }
        }
    }
}

impl From<io::Error> for SollyaError {
    fn from(err: io::Error) -> Self {
        SollyaError::Io(err)
    }
}

impl From<FromUtf8Error> for SollyaError {
    fn from(_: FromUtf8Error) -> Self {
        SollyaError::Utf8
    }
}

/// A built-in function recognized by Sollya.
#[derive(Clone, Copy, Debug, Mangle)]
#[rustfmt::skip]
pub enum SollyaFunction {
    Sin,  Cos,   Tan,  Sinh,  Cosh,  Tanh,
    ASin, ACos,  ATan, ASinh, ACosh, ATanh,
    Exp,  ExpM1, Log,  Log2,  Log10, Log1P,
    Sqrt, Erf,   ErfC,
}

impl SollyaFunction {
    pub fn as_str(&self) -> &'static str {
        match self {
            SollyaFunction::Sin => "sin",
            SollyaFunction::Cos => "cos",
            SollyaFunction::Tan => "tan",
            SollyaFunction::Sinh => "sinh",
            SollyaFunction::Cosh => "cosh",
            SollyaFunction::Tanh => "tanh",
            SollyaFunction::ASin => "asin",
            SollyaFunction::ACos => "acos",
            SollyaFunction::ATan => "atan",
            SollyaFunction::ASinh => "asinh",
            SollyaFunction::ACosh => "acosh",
            SollyaFunction::ATanh => "atanh",
            SollyaFunction::Exp => "exp",
            SollyaFunction::ExpM1 => "expm1",
            SollyaFunction::Log => "log",
            SollyaFunction::Log2 => "log2",
            SollyaFunction::Log10 => "log10",
            SollyaFunction::Log1P => "log1p",
            SollyaFunction::Sqrt => "sqrt",
            SollyaFunction::Erf => "erf",
            SollyaFunction::ErfC => "erfc",
        }
    }
}

impl fmt::Display for SollyaFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl TryFrom<ast::MathOp> for SollyaFunction {
    type Error = ();

    fn try_from(value: ast::MathOp) -> Result<Self, Self::Error> {
        match value {
            ast::MathOp::Sin => Ok(SollyaFunction::Sin),
            ast::MathOp::Cos => Ok(SollyaFunction::Cos),
            ast::MathOp::Tan => Ok(SollyaFunction::Tan),
            ast::MathOp::Sinh => Ok(SollyaFunction::Sinh),
            ast::MathOp::Cosh => Ok(SollyaFunction::Cosh),
            ast::MathOp::Tanh => Ok(SollyaFunction::Tanh),
            ast::MathOp::ASin => Ok(SollyaFunction::ASin),
            ast::MathOp::ACos => Ok(SollyaFunction::ACos),
            ast::MathOp::ATan => Ok(SollyaFunction::ATan),
            ast::MathOp::ASinh => Ok(SollyaFunction::ASinh),
            ast::MathOp::ACosh => Ok(SollyaFunction::ACosh),
            ast::MathOp::ATanh => Ok(SollyaFunction::ATanh),
            ast::MathOp::Exp => Ok(SollyaFunction::Exp),
            ast::MathOp::ExpM1 => Ok(SollyaFunction::ExpM1),
            ast::MathOp::Log => Ok(SollyaFunction::Log),
            ast::MathOp::Log2 => Ok(SollyaFunction::Log2),
            ast::MathOp::Log10 => Ok(SollyaFunction::Log10),
            ast::MathOp::Log1P => Ok(SollyaFunction::Log1P),
            ast::MathOp::Sqrt => Ok(SollyaFunction::Sqrt),
            ast::MathOp::Erf => Ok(SollyaFunction::Erf),
            ast::MathOp::ErfC => Ok(SollyaFunction::ErfC),
            _ => Err(()),
        }
    }
}
