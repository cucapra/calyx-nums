//! Implementation of elementary functions.

pub mod addressing;
pub mod datapath;
pub mod remez;

pub use addressing::{AddressSpec, TableDomain};
pub use datapath::{Datapath, HornerRanges};
