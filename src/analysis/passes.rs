use std::cell::OnceCell;

use calyx_utils::CalyxResult;

use super::{ContextResolution, TypeCheck};
use crate::fpcore::ast;
use crate::opts::Opts;

pub trait Managed<Cache>
where
    Self: Sized,
{
    fn get(cache: &Cache) -> &OnceCell<Self>;
}

macro_rules! register_passes {
    ($cache:ident<$ast:lifetime> { $( $field:ident: $pass:ty ),* $(,)? }) => {
        #[derive(Default)]
        pub struct $cache<$ast> {
            $( $field: OnceCell<$pass> ),*
        }

        $(
            impl<$ast> Managed<$cache<$ast>> for $pass {
                fn get<'pm>(cache: &'pm $cache<$ast>) -> &'pm OnceCell<Self> {
                    &cache.$field
                }
            }
        )*
    };
}

register_passes!(Cache<'ast> {
    context: ContextResolution<'ast>,
    types: TypeCheck,
});

pub trait Pass<'ast>
where
    Self: Sized,
{
    fn run(pm: &PassManager<'_, 'ast>) -> CalyxResult<Self>;
}

pub struct PassManager<'pm, 'ast> {
    opts: &'pm Opts,
    defs: &'ast [ast::BenchmarkDef],
    cache: Cache<'ast>,
}

impl<'pm, 'ast> PassManager<'pm, 'ast> {
    pub fn new(
        opts: &'pm Opts,
        defs: &'ast [ast::BenchmarkDef],
    ) -> PassManager<'pm, 'ast> {
        PassManager {
            opts,
            defs,
            cache: Default::default(),
        }
    }

    pub fn opts(&self) -> &'pm Opts {
        self.opts
    }

    pub fn ast(&self) -> &'ast [ast::BenchmarkDef] {
        self.defs
    }

    pub fn get_analysis<A>(&self) -> CalyxResult<&A>
    where
        A: Pass<'ast> + Managed<Cache<'ast>>,
    {
        let cell = A::get(&self.cache);

        if let Some(val) = cell.get() {
            Ok(val)
        } else {
            let _ = cell.set(A::run(self)?);

            Ok(cell.get().unwrap())
        }
    }
}
