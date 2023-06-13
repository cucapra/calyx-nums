//! Interface to the Sollya tool.

use std::ffi::OsStr;
use std::io::{self, Write};
use std::process::{Command, Stdio};
use std::string::FromUtf8Error;
use std::{fmt, thread};

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
            SollyaError::Io(err) => write!(f, "I/O error: {err}"),
            SollyaError::Utf8 => {
                write!(f, "invocation returned invalid UTF-8")
            }
            SollyaError::Status(status) => {
                write!(f, "invocation failed: {status}")
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
