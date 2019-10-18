use crate::semantic::types::{MonoType, Tvar};
use std::{collections::HashMap, fmt};

// A substitution is a function that takes a monotype as input
// and returns an equivalent monotype as output.
//
// More formally a substitution is a function S: X -> Y such that
// X = {set of all monotypes} and Y is a subset of X.
//
// A substitution is therefore a function that can be applied
// to monotypes.
//
pub trait Substitutable {
    fn apply(self, sub: &Subst) -> Self;
}

// A Substitution is implemented as a map from type variables
// to monotypes.
//
// Note this implementation implies that substitutions are
// idempotent. Given a monotype x, and a substitution s, s(s(x)) = s(x).
//
#[derive(Debug)]
pub struct Subst(HashMap<Tvar, MonoType>);

impl fmt::Display for Subst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("substitution:\n")?;
        for (k, v) in &self.0 {
            write!(f, "\t{}: {}\n", k, v)?;
        }
        Ok(())
    }
}

impl Subst {
    pub fn empty() -> Subst {
        Subst(HashMap::new())
    }

    pub fn lookup(&self, tv: Tvar) -> Option<&MonoType> {
        self.0.get(&tv)
    }

    pub fn merge(self, with: Subst) -> Subst {
        let mut values = HashMap::with_capacity(self.0.len() + with.0.len());
        for (k, v) in self.0 {
            values.insert(k, v.apply(&with));
        }
        for (k, v) in with.0 {
            values.insert(k, v);
        }
        Subst(values)
    }
}
