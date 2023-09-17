use ::cranelift::prelude::*;
use clap::{Parser, ValueEnum};

pub mod cranelift;

#[derive(Clone, Copy, Debug, Parser, ValueEnum)]
pub enum OptLevel {
    #[clap(name = "0")]
    None,
    #[clap(name = "s")]
    Speed,
    #[clap(name = "ss")]
    SpeedAndSize,
}

impl std::fmt::Display for OptLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OptLevel::None => write!(f, "none"),
            OptLevel::Speed => write!(f, "speed"),
            OptLevel::SpeedAndSize => write!(f, "speed_and_size"),
        }
    }
}

impl From<settings::OptLevel> for OptLevel {
    fn from(opt: settings::OptLevel) -> Self {
        match opt {
            settings::OptLevel::None => OptLevel::None,
            settings::OptLevel::Speed => OptLevel::Speed,
            settings::OptLevel::SpeedAndSize => OptLevel::SpeedAndSize,
        }
    }
}
