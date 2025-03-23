use std::cmp::{self, Ordering};
use std::collections::HashMap;

use itertools::Itertools;

use super::{Binding, NameResolution};
use crate::fpcore::ast;
use crate::utils::{Diagnostic, Reporter};

#[derive(Debug)]
pub struct UnsupportedPredicateError;

#[derive(Default)]
pub struct Precondition<'ast> {
    pub domains: HashMap<ast::NodeId, Interval<'ast>>,
}

impl<'ast> Precondition<'ast> {
    pub fn new() -> Precondition<'ast> {
        Default::default()
    }

    pub fn add_constraint(
        &mut self,
        pred: &'ast ast::Expression,
        bindings: &NameResolution,
        reporter: &mut Reporter,
    ) -> Result<(), UnsupportedPredicateError> {
        let mut unsupported = || {
            reporter.emit(
                &Diagnostic::error()
                    .with_message("unsupported precondition")
                    .with_primary(pred.span, "not allowed here"),
            );

            Err(UnsupportedPredicateError)
        };

        let ast::ExprKind::Op(op, args) = &pred.kind else {
            return unsupported();
        };

        let ast::OpKind::Test(op) = op.kind else {
            return unsupported();
        };

        match op {
            ast::TestOp::And => {
                for arg in args {
                    self.add_constraint(arg, bindings, reporter)?;
                }
            }
            ast::TestOp::Or => {
                let mut disjunction = Precondition::new();

                disjunction.add_constraint(&args[0], bindings, reporter)?;

                for arg in &args[1..] {
                    let mut disjunct = Precondition::new();

                    disjunct.add_constraint(arg, bindings, reporter)?;
                    disjunction.union(disjunct);
                }

                self.intersect(disjunction);
            }
            ast::TestOp::Lt
            | ast::TestOp::Gt
            | ast::TestOp::Leq
            | ast::TestOp::Geq => {
                for (a, b) in args.iter().tuple_windows() {
                    let (a, b) =
                        if matches!(op, ast::TestOp::Lt | ast::TestOp::Leq) {
                            (a, b)
                        } else {
                            (b, a)
                        };

                    let (var, domain) = match (&a.kind, &b.kind) {
                        (ast::ExprKind::Id(_), ast::ExprKind::Num(num)) => {
                            let domain = Interval::new(
                                Endpoint::Infinite,
                                Endpoint::Finite(&num.value),
                            );

                            (a.uid, domain)
                        }
                        (ast::ExprKind::Num(num), ast::ExprKind::Id(_)) => {
                            let domain = Interval::new(
                                Endpoint::Finite(&num.value),
                                Endpoint::Infinite,
                            );

                            (b.uid, domain)
                        }
                        _ => {
                            return unsupported();
                        }
                    };

                    let Binding::Argument(arg) = bindings.names[&var] else {
                        unreachable!("`:pre` only checked at top level");
                    };

                    self.domains
                        .entry(arg.uid)
                        .and_modify(|old| *old = old.intersection(&domain))
                        .or_insert(domain);
                }
            }
            _ => {
                return unsupported();
            }
        }

        Ok(())
    }

    fn union(&mut self, other: Precondition<'ast>) {
        self.domains = self
            .domains
            .iter()
            .filter_map(|(uid, left)| {
                other
                    .domains
                    .get(uid)
                    .map(|right| (*uid, left.union(right)))
            })
            .collect();
    }

    fn intersect(&mut self, other: Precondition<'ast>) {
        for (uid, right) in other.domains {
            self.domains
                .entry(uid)
                .and_modify(|left| *left = left.intersection(&right))
                .or_insert(right);
        }
    }
}

pub struct Interval<'a> {
    left: Left<'a>,
    right: Right<'a>,
}

impl<'a> Interval<'a> {
    fn new(left: Endpoint<'a>, right: Endpoint<'a>) -> Interval<'a> {
        Interval {
            left: Left(left),
            right: Right(right),
        }
    }

    pub fn union(&self, other: &Interval<'a>) -> Interval<'a> {
        Interval {
            left: cmp::min(self.left, other.left),
            right: cmp::max(self.right, other.right),
        }
    }

    pub fn intersection(&self, other: &Interval<'a>) -> Interval<'a> {
        Interval {
            left: cmp::max(self.left, other.left),
            right: cmp::min(self.right, other.right),
        }
    }

    pub fn bounds(&self) -> Option<(&'a ast::Rational, &'a ast::Rational)> {
        match (self.left.0, self.right.0) {
            (Endpoint::Finite(left), Endpoint::Finite(right)) => {
                Some((left, right))
            }
            _ => None,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Endpoint<'a> {
    Infinite,
    Finite(&'a ast::Rational),
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct Left<'a>(Endpoint<'a>);

impl PartialOrd for Left<'_> {
    fn partial_cmp(&self, other: &Left) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Left<'_> {
    fn cmp(&self, other: &Left) -> Ordering {
        match (self.0, other.0) {
            (Endpoint::Infinite, Endpoint::Infinite) => Ordering::Equal,
            (Endpoint::Infinite, Endpoint::Finite(_)) => Ordering::Less,
            (Endpoint::Finite(_), Endpoint::Infinite) => Ordering::Greater,
            (Endpoint::Finite(a), Endpoint::Finite(b)) => a.cmp(b),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct Right<'a>(Endpoint<'a>);

impl PartialOrd for Right<'_> {
    fn partial_cmp(&self, other: &Right) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Right<'_> {
    fn cmp(&self, other: &Right) -> Ordering {
        match (self.0, other.0) {
            (Endpoint::Infinite, Endpoint::Infinite) => Ordering::Equal,
            (Endpoint::Infinite, Endpoint::Finite(_)) => Ordering::Greater,
            (Endpoint::Finite(_), Endpoint::Infinite) => Ordering::Less,
            (Endpoint::Finite(a), Endpoint::Finite(b)) => a.cmp(b),
        }
    }
}
