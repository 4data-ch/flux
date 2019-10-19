use crate::semantic::sub::{Subst, Substitutable};
use std::{
    cmp,
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    fmt, result,
};

// PolyType represents a generic parametrized type.
//
// TODO:
//     Do not derive PartialEq implementation.
//     Instead provide a custom implementation
//     that instantiates both polytypes with the
//     same type variables.
//
//     Note this requires a substitution, so remove
//     this derivation once substitutions are defined.
//
#[derive(Debug, Clone, PartialEq)]
pub struct PolyType {
    pub free: Vec<Tvar>,
    pub bnds: Option<Const>,
    pub expr: MonoType,
}

impl fmt::Display for PolyType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.bnds {
            Some(bounds) => write!(
                f,
                "forall [{}] where {} {}",
                DisplayList {
                    values: self.free.to_owned(),
                    delim: ", "
                },
                bounds,
                self.expr,
            ),
            None => write!(
                f,
                "forall [{}] {}",
                DisplayList {
                    values: self.free.to_owned(),
                    delim: ", "
                },
                self.expr
            ),
        }
    }
}

// Kind constraints on type variables.
//
// Ex. forall [t0] where t0:Comparable + Equatable (x:t0, y:t0) -> bool
//                       ^_______________________^
//
#[derive(Debug, Clone, PartialEq)]
pub struct Const {
    pub kinds: HashMap<Tvar, HashSet<Kind>>,
}

impl fmt::Display for Const {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        DisplayList {
            values: self
                .kinds
                .iter()
                .collect::<BTreeMap<_, _>>()
                .iter()
                .map(|(&tv, &kinds)| TvarConst { tv, kinds })
                .collect(),
            delim: ", ",
        }
        .fmt(f)
    }
}

// Private struct whose sole purpose is for displaying the generic
// constraints for the free variables of a polytype.
#[derive(Debug)]
struct TvarConst<'a> {
    tv: &'a Tvar,
    kinds: &'a HashSet<Kind>,
}

impl<'a> fmt::Display for TvarConst<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}:{}",
            self.tv,
            DisplayList {
                values: self.kinds.iter().collect::<BTreeSet<_>>().iter().collect(),
                delim: " + ",
            },
        )
    }
}

// Unification equates two types by producing a substitution whereby
// the types are equal after application of the substitution.
//
// More formally, s = unify(x,y) => s(x) = s(y) .
//
// Note here that unification also takes a set of constrained type
// variables and produces a possibly updated set of constraints along
// with the substitution equating the two types.
//
trait Unifiable {
    fn unify(self, with: Self, cons: Const) -> Result;
}

// Types may be constrained with Kinds.
//
// Unlike unification which asserts an equality constraint on two
// types, a kind constraint asserts the containment of a type within
// a type class or family.
//
trait Constrainable {
    fn constrain(self, with: Kind, cons: Const) -> Result;
}

type Result = result::Result<(Subst, Const), Error>;

#[derive(Debug)]
enum Error {
    Constrain { t: MonoType, with: Kind },
    Unify { t: MonoType, with: MonoType },
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Constrain { t, with } => write!(f, "{} is not of kind {}", t, with),
            Error::Unify { t, with } => write!(f, "cannot unify {} with {}", t, with),
        }
    }
}

// Kind represents a class or family of types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Kind {
    Addable,
    Subtractable,
    Divisible,
    Comparable,
    Equatable,
    Nullable,
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Kind::Addable => f.write_str("Addable"),
            Kind::Subtractable => f.write_str("Subtractable"),
            Kind::Divisible => f.write_str("Divisible"),
            Kind::Comparable => f.write_str("Comparable"),
            Kind::Equatable => f.write_str("Equatable"),
            Kind::Nullable => f.write_str("Nullable"),
        }
    }
}

impl cmp::Ord for Kind {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.to_string().cmp(&other.to_string())
    }
}

impl cmp::PartialOrd for Kind {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

// MonoType represents a specific named type
#[derive(Debug, Clone, PartialEq)]
pub enum MonoType {
    Bool(Bool),
    Int(Int),
    Uint(Uint),
    Float(Float),
    String(Str),
    Duration(Duration),
    Time(Time),
    Regexp(Regexp),
    Var(Tvar),
    Arr(Box<Array>),
    Row(Box<Row>),
    Fun(Box<Function>),
}

impl fmt::Display for MonoType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MonoType::Bool(t) => t.fmt(f),
            MonoType::Int(t) => t.fmt(f),
            MonoType::Uint(t) => t.fmt(f),
            MonoType::Float(t) => t.fmt(f),
            MonoType::String(t) => t.fmt(f),
            MonoType::Duration(t) => t.fmt(f),
            MonoType::Time(t) => t.fmt(f),
            MonoType::Regexp(t) => t.fmt(f),
            MonoType::Var(var) => var.fmt(f),
            MonoType::Arr(arr) => arr.fmt(f),
            MonoType::Row(obj) => obj.fmt(f),
            MonoType::Fun(fun) => fun.fmt(f),
        }
    }
}

impl Substitutable for MonoType {
    fn apply(self, sub: &Subst) -> Self {
        match self {
            MonoType::Bool(t) => MonoType::Bool(t.apply(sub)),
            MonoType::Int(t) => MonoType::Int(t.apply(sub)),
            MonoType::Uint(t) => MonoType::Uint(t.apply(sub)),
            MonoType::Float(t) => MonoType::Float(t.apply(sub)),
            MonoType::String(t) => MonoType::String(t.apply(sub)),
            MonoType::Duration(t) => MonoType::Duration(t.apply(sub)),
            MonoType::Time(t) => MonoType::Time(t.apply(sub)),
            MonoType::Regexp(t) => MonoType::Regexp(t.apply(sub)),
            MonoType::Var(tvr) => tvr.apply(sub),
            MonoType::Arr(arr) => MonoType::Arr(Box::new(arr.apply(sub))),
            MonoType::Row(obj) => MonoType::Row(Box::new(obj.apply(sub))),
            MonoType::Fun(fun) => MonoType::Fun(Box::new(fun.apply(sub))),
        }
    }
}

impl Unifiable for MonoType {
    fn unify(self, with: Self, cons: Const) -> Result {
        match self {
            MonoType::Bool(t) => {
                if let MonoType::Var(tv) = with {
                    tv.unify(self, cons)
                } else if let MonoType::Bool(s) = with {
                    t.unify(s, cons)
                } else {
                    Err(Error::Unify { t: self, with })
                }
            }
            MonoType::Int(t) => {
                if let MonoType::Var(tv) = with {
                    tv.unify(self, cons)
                } else if let MonoType::Int(s) = with {
                    t.unify(s, cons)
                } else {
                    Err(Error::Unify { t: self, with })
                }
            }
            MonoType::Uint(t) => {
                if let MonoType::Var(tv) = with {
                    tv.unify(self, cons)
                } else if let MonoType::Uint(s) = with {
                    t.unify(s, cons)
                } else {
                    Err(Error::Unify { t: self, with })
                }
            }
            MonoType::Float(t) => {
                if let MonoType::Var(tv) = with {
                    tv.unify(self, cons)
                } else if let MonoType::Float(s) = with {
                    t.unify(s, cons)
                } else {
                    Err(Error::Unify { t: self, with })
                }
            }
            MonoType::String(t) => {
                if let MonoType::Var(tv) = with {
                    tv.unify(self, cons)
                } else if let MonoType::String(s) = with {
                    t.unify(s, cons)
                } else {
                    Err(Error::Unify { t: self, with })
                }
            }
            MonoType::Duration(t) => {
                if let MonoType::Var(tv) = with {
                    tv.unify(self, cons)
                } else if let MonoType::Duration(s) = with {
                    t.unify(s, cons)
                } else {
                    Err(Error::Unify { t: self, with })
                }
            }
            MonoType::Time(t) => {
                if let MonoType::Var(tv) = with {
                    tv.unify(self, cons)
                } else if let MonoType::Time(s) = with {
                    t.unify(s, cons)
                } else {
                    Err(Error::Unify { t: self, with })
                }
            }
            MonoType::Regexp(t) => {
                if let MonoType::Var(tv) = with {
                    tv.unify(self, cons)
                } else if let MonoType::Regexp(s) = with {
                    t.unify(s, cons)
                } else {
                    Err(Error::Unify { t: self, with })
                }
            }
            MonoType::Arr(t) => {
                if let MonoType::Var(tv) = with {
                    tv.unify(MonoType::Arr(t), cons)
                } else if let MonoType::Arr(arr) = with {
                    t.unify(*arr, cons)
                } else {
                    Err(Error::Unify {
                        t: MonoType::Arr(t),
                        with,
                    })
                }
            }
            MonoType::Row(t) => {
                if let MonoType::Var(tv) = with {
                    tv.unify(MonoType::Row(t), cons)
                } else if let MonoType::Row(obj) = with {
                    t.unify(*obj, cons)
                } else {
                    Err(Error::Unify {
                        t: MonoType::Row(t),
                        with,
                    })
                }
            }
            MonoType::Fun(t) => {
                if let MonoType::Var(tv) = with {
                    tv.unify(MonoType::Fun(t), cons)
                } else if let MonoType::Fun(fun) = with {
                    t.unify(*fun, cons)
                } else {
                    Err(Error::Unify {
                        t: MonoType::Fun(t),
                        with,
                    })
                }
            }
            MonoType::Var(t) => t.unify(with, cons),
        }
    }
}

impl Constrainable for MonoType {
    fn constrain(self, with: Kind, cons: Const) -> Result {
        match self {
            MonoType::Bool(t) => t.constrain(with, cons),
            MonoType::Int(t) => t.constrain(with, cons),
            MonoType::Uint(t) => t.constrain(with, cons),
            MonoType::Float(t) => t.constrain(with, cons),
            MonoType::String(t) => t.constrain(with, cons),
            MonoType::Duration(t) => t.constrain(with, cons),
            MonoType::Time(t) => t.constrain(with, cons),
            MonoType::Regexp(t) => t.constrain(with, cons),
            MonoType::Var(tvr) => tvr.constrain(with, cons),
            MonoType::Arr(arr) => arr.constrain(with, cons),
            MonoType::Row(obj) => obj.constrain(with, cons),
            MonoType::Fun(fun) => fun.constrain(with, cons),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Bool;

impl fmt::Display for Bool {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("bool")
    }
}

impl Substitutable for Bool {
    fn apply(self, _: &Subst) -> Self {
        self
    }
}

impl Unifiable for Bool {
    fn unify(self, _: Self, cons: Const) -> Result {
        Ok((Subst::empty(), cons))
    }
}

impl Constrainable for Bool {
    fn constrain(self, with: Kind, cons: Const) -> Result {
        match with {
            Kind::Equatable | Kind::Nullable => Ok((Subst::empty(), cons)),
            _ => Err(Error::Constrain {
                t: MonoType::Bool(self),
                with,
            }),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Int;

impl fmt::Display for Int {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("int")
    }
}

impl Substitutable for Int {
    fn apply(self, _: &Subst) -> Self {
        self
    }
}

impl Unifiable for Int {
    fn unify(self, _: Self, cons: Const) -> Result {
        Ok((Subst::empty(), cons))
    }
}

impl Constrainable for Int {
    fn constrain(self, with: Kind, cons: Const) -> Result {
        match with {
            Kind::Addable
            | Kind::Subtractable
            | Kind::Divisible
            | Kind::Comparable
            | Kind::Equatable
            | Kind::Nullable => Ok((Subst::empty(), cons)),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Uint;

impl fmt::Display for Uint {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("uint")
    }
}

impl Substitutable for Uint {
    fn apply(self, _: &Subst) -> Self {
        self
    }
}

impl Unifiable for Uint {
    fn unify(self, _: Self, cons: Const) -> Result {
        Ok((Subst::empty(), cons))
    }
}

impl Constrainable for Uint {
    fn constrain(self, with: Kind, cons: Const) -> Result {
        match with {
            Kind::Addable
            | Kind::Divisible
            | Kind::Comparable
            | Kind::Equatable
            | Kind::Nullable => Ok((Subst::empty(), cons)),
            _ => Err(Error::Constrain {
                t: MonoType::Uint(self),
                with,
            }),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Float;

impl fmt::Display for Float {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("float")
    }
}

impl Substitutable for Float {
    fn apply(self, _: &Subst) -> Self {
        self
    }
}

impl Unifiable for Float {
    fn unify(self, _: Self, cons: Const) -> Result {
        Ok((Subst::empty(), cons))
    }
}

impl Constrainable for Float {
    fn constrain(self, with: Kind, cons: Const) -> Result {
        match with {
            Kind::Addable
            | Kind::Subtractable
            | Kind::Divisible
            | Kind::Comparable
            | Kind::Equatable
            | Kind::Nullable => Ok((Subst::empty(), cons)),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Str;

impl fmt::Display for Str {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("string")
    }
}

impl Substitutable for Str {
    fn apply(self, _: &Subst) -> Self {
        self
    }
}

impl Unifiable for Str {
    fn unify(self, _: Self, cons: Const) -> Result {
        Ok((Subst::empty(), cons))
    }
}

impl Constrainable for Str {
    fn constrain(self, with: Kind, cons: Const) -> Result {
        match with {
            Kind::Addable | Kind::Comparable | Kind::Equatable | Kind::Nullable => {
                Ok((Subst::empty(), cons))
            }
            _ => Err(Error::Constrain {
                t: MonoType::String(self),
                with,
            }),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Duration;

impl fmt::Display for Duration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("duration")
    }
}

impl Substitutable for Duration {
    fn apply(self, _: &Subst) -> Self {
        self
    }
}

impl Unifiable for Duration {
    fn unify(self, _: Self, cons: Const) -> Result {
        Ok((Subst::empty(), cons))
    }
}

impl Constrainable for Duration {
    fn constrain(self, with: Kind, cons: Const) -> Result {
        match with {
            Kind::Comparable | Kind::Equatable | Kind::Nullable => Ok((Subst::empty(), cons)),
            _ => Err(Error::Constrain {
                t: MonoType::Duration(self),
                with,
            }),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Time;

impl fmt::Display for Time {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("time")
    }
}

impl Substitutable for Time {
    fn apply(self, _: &Subst) -> Self {
        self
    }
}

impl Unifiable for Time {
    fn unify(self, _: Self, cons: Const) -> Result {
        Ok((Subst::empty(), cons))
    }
}

impl Constrainable for Time {
    fn constrain(self, with: Kind, cons: Const) -> Result {
        match with {
            Kind::Comparable | Kind::Equatable | Kind::Nullable => Ok((Subst::empty(), cons)),
            _ => Err(Error::Constrain {
                t: MonoType::Time(self),
                with,
            }),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Regexp;

impl fmt::Display for Regexp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("regexp")
    }
}

impl Substitutable for Regexp {
    fn apply(self, _: &Subst) -> Self {
        self
    }
}

impl Unifiable for Regexp {
    fn unify(self, _: Self, cons: Const) -> Result {
        Ok((Subst::empty(), cons))
    }
}

impl Constrainable for Regexp {
    fn constrain(self, with: Kind, _: Const) -> Result {
        Err(Error::Constrain {
            t: MonoType::Regexp(self),
            with,
        })
    }
}

// Tvar stands for type variable.
// A type variable holds an unknown type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Tvar(pub i64);

impl fmt::Display for Tvar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "t{}", self.0)
    }
}

impl Constrainable for Tvar {
    fn constrain(self, _: Kind, _: Const) -> Result {
        unimplemented!();
    }
}

impl Tvar {
    fn apply(self, sub: &Subst) -> MonoType {
        match sub.lookup(self) {
            Some(t) => t.clone(),
            None => MonoType::Var(self),
        }
    }

    fn unify(self, _: MonoType, _: Const) -> Result {
        unimplemented!();
    }
}

// Array is a homogeneous list type
#[derive(Debug, Clone, PartialEq)]
pub struct Array(pub MonoType);

impl fmt::Display for Array {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}]", self.0)
    }
}

impl Substitutable for Array {
    fn apply(self, sub: &Subst) -> Self {
        Array(self.0.apply(sub))
    }
}

impl Unifiable for Array {
    fn unify(self, with: Self, cons: Const) -> Result {
        self.0.unify(with.0, cons)
    }
}

impl Constrainable for Box<Array> {
    fn constrain(self, with: Kind, _: Const) -> Result {
        Err(Error::Constrain {
            t: MonoType::Arr(self),
            with,
        })
    }
}

// Row is an extensible record type.
//
// A row is either Empty meaning it has no properties,
// or it is an extension of a row.
//
// A row may extend what is referred to as a row
// variable. A row variable is a type variable that
// represents an unknown record type.
//
#[derive(Debug, Clone)]
pub enum Row {
    Empty,
    Extension { head: Property, tail: MonoType },
}

impl fmt::Display for Row {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("{")?;
        self.display(f)?;
        f.write_str("}")
    }
}

impl cmp::PartialEq for Row {
    fn eq(&self, other: &Self) -> bool {
        let mut l = HashMap::new();
        let mut r = HashMap::new();
        self.flatten(&mut l) == other.flatten(&mut r) && l == r
    }
}

impl Substitutable for Row {
    fn apply(self, sub: &Subst) -> Self {
        match self {
            Row::Empty => Row::Empty,
            Row::Extension { head, tail } => Row::Extension {
                head: head.apply(sub),
                tail: tail.apply(sub),
            },
        }
    }
}

impl Unifiable for Row {
    fn unify(self, _: Self, _: Const) -> Result {
        unimplemented!();
    }
}

impl Constrainable for Box<Row> {
    fn constrain(self, with: Kind, _: Const) -> Result {
        Err(Error::Constrain {
            t: MonoType::Row(self),
            with,
        })
    }
}

impl Row {
    // Display a row type in flattened format
    fn display(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Row::Empty => f.write_str("{}"),
            Row::Extension { head, tail } => {
                write!(f, "{} | ", head)?;
                match tail {
                    MonoType::Var(tvr) => write!(f, "{}", tvr),
                    MonoType::Row(obj) => obj.display(f),
                    _ => Err(fmt::Error),
                }
            }
        }
    }

    // Flatten a record type into a hashmap of property names and types
    fn flatten(&self, props: &mut HashMap<String, MonoType>) -> Option<Tvar> {
        match self {
            Row::Empty => None,
            Row::Extension { head, tail } => {
                props.insert(head.k.clone(), head.v.clone());
                match tail {
                    MonoType::Row(obj) => obj.flatten(props),
                    MonoType::Var(tvr) => Some(*tvr),
                    _ => panic!("tail of row must be either a row variable or another row"),
                }
            }
        }
    }
}

// A key value pair representing a property type in a record
#[derive(Debug, Clone, PartialEq)]
pub struct Property {
    pub k: String,
    pub v: MonoType,
}

impl fmt::Display for Property {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.k, self.v)
    }
}

impl Substitutable for Property {
    fn apply(self, sub: &Subst) -> Self {
        Property {
            k: self.k,
            v: self.v.apply(sub),
        }
    }
}

// Function represents a function type.
//
// A function type is defined by as set of required arguments,
// a set of optional arguments, an optional pipe argument, and
// a required return type.
//
#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub req: HashMap<String, MonoType>,
    pub opt: HashMap<String, MonoType>,
    pub pipe: Option<Property>,
    pub retn: MonoType,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let required = self
            .req
            .iter()
            .collect::<BTreeMap<_, _>>()
            .iter()
            .map(|(&k, &v)| Property {
                k: k.clone(),
                v: v.clone(),
            })
            .collect::<Vec<_>>();

        let optional = self
            .opt
            .iter()
            .collect::<BTreeMap<_, _>>()
            .iter()
            .map(|(&k, &v)| Property {
                k: String::from("?") + &k,
                v: v.clone(),
            })
            .collect::<Vec<_>>();

        let pipe = match &self.pipe {
            Some(pipe) => {
                if pipe.k == "<-" {
                    vec![pipe.clone()]
                } else {
                    vec![Property {
                        k: String::from("<-") + &pipe.k,
                        v: pipe.v.clone(),
                    }]
                }
            }
            None => vec![],
        };

        let values = pipe
            .iter()
            .chain(required.iter().chain(optional.iter()))
            .collect();

        write!(
            f,
            "({}) -> {}",
            DisplayList {
                values: values,
                delim: ", ",
            },
            self.retn
        )
    }
}

impl Substitutable for HashMap<String, MonoType> {
    fn apply(self, sub: &Subst) -> Self {
        let mut args = self;
        for (_, v) in &mut args {
            *v = v.clone().apply(sub);
        }
        args
    }
}

impl Substitutable for Function {
    fn apply(self, sub: &Subst) -> Self {
        Function {
            req: self.req.apply(sub),
            opt: self.opt.apply(sub),
            pipe: match self.pipe {
                None => None,
                Some(p) => Some(p.apply(sub)),
            },
            retn: self.retn.apply(sub),
        }
    }
}

impl Unifiable for Function {
    fn unify(self, _: Self, _: Const) -> Result {
        unimplemented!();
    }
}

impl Constrainable for Box<Function> {
    fn constrain(self, with: Kind, _: Const) -> Result {
        Err(Error::Constrain {
            t: MonoType::Fun(self),
            with,
        })
    }
}

// DisplayList is a list of elements each of which can be displayed
#[derive(Debug, Clone, PartialEq)]
struct DisplayList<T> {
    values: Vec<T>,
    delim: &'static str,
}

impl<T: fmt::Display> fmt::Display for DisplayList<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.values.is_empty() {
            return Ok(());
        }
        let size = self.values.len();
        let list = &self.values[..size - 1];
        for v in list {
            write!(f, "{}{}", v, self.delim)?;
        }
        self.values[size - 1].fmt(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn display_kind_addable() {
        assert!(Kind::Addable.to_string() == "Addable");
    }
    #[test]
    fn display_kind_subtractable() {
        assert!(Kind::Subtractable.to_string() == "Subtractable");
    }
    #[test]
    fn display_kind_divisible() {
        assert!(Kind::Divisible.to_string() == "Divisible");
    }
    #[test]
    fn display_kind_comparable() {
        assert!(Kind::Comparable.to_string() == "Comparable");
    }
    #[test]
    fn display_kind_equatable() {
        assert!(Kind::Equatable.to_string() == "Equatable");
    }
    #[test]
    fn display_kind_nullable() {
        assert!(Kind::Nullable.to_string() == "Nullable");
    }

    #[test]
    fn display_type_bool() {
        assert_eq!("bool", Bool.to_string());
    }
    #[test]
    fn display_type_int() {
        assert_eq!("int", Int.to_string());
    }
    #[test]
    fn display_type_uint() {
        assert_eq!("uint", Uint.to_string());
    }
    #[test]
    fn display_type_float() {
        assert_eq!("float", Float.to_string());
    }
    #[test]
    fn display_type_string() {
        assert_eq!("string", Str.to_string());
    }
    #[test]
    fn display_type_duration() {
        assert_eq!("duration", Duration.to_string());
    }
    #[test]
    fn display_type_time() {
        assert_eq!("time", Time.to_string());
    }
    #[test]
    fn display_type_regexp() {
        assert_eq!("regexp", Regexp.to_string());
    }
    #[test]
    fn display_type_tvar() {
        assert_eq!("t10", Tvar(10).to_string());
    }
    #[test]
    fn display_type_array() {
        assert_eq!(
            "[int]",
            MonoType::Arr(Box::new(Array(MonoType::Int(Int)))).to_string()
        );
    }
    #[test]
    fn display_type_row() {
        assert_eq!(
            "{a:int | b:string | t0}",
            Row::Extension {
                head: Property {
                    k: String::from("a"),
                    v: MonoType::Int(Int),
                },
                tail: MonoType::Row(Box::new(Row::Extension {
                    head: Property {
                        k: String::from("b"),
                        v: MonoType::String(Str),
                    },
                    tail: MonoType::Var(Tvar(0)),
                })),
            }
            .to_string()
        );
        assert_eq!(
            "{a:int | b:string | {}}",
            Row::Extension {
                head: Property {
                    k: String::from("a"),
                    v: MonoType::Int(Int),
                },
                tail: MonoType::Row(Box::new(Row::Extension {
                    head: Property {
                        k: String::from("b"),
                        v: MonoType::String(Str),
                    },
                    tail: MonoType::Row(Box::new(Row::Empty)),
                })),
            }
            .to_string()
        );
    }
    #[test]
    fn display_type_function() {
        assert_eq!(
            "() -> int",
            Function {
                req: HashMap::new(),
                opt: HashMap::new(),
                pipe: None,
                retn: MonoType::Int(Int),
            }
            .to_string()
        );
        assert_eq!(
            "(<-:int) -> int",
            Function {
                req: HashMap::new(),
                opt: HashMap::new(),
                pipe: Some(Property {
                    k: String::from("<-"),
                    v: MonoType::Int(Int),
                }),
                retn: MonoType::Int(Int),
            }
            .to_string()
        );
        assert_eq!(
            "(<-a:int) -> int",
            Function {
                req: HashMap::new(),
                opt: HashMap::new(),
                pipe: Some(Property {
                    k: String::from("a"),
                    v: MonoType::Int(Int),
                }),
                retn: MonoType::Int(Int),
            }
            .to_string()
        );
        assert_eq!(
            "(<-:int, a:int, b:int) -> int",
            Function {
                req: maplit::hashmap! {
                    String::from("a") => MonoType::Int(Int),
                    String::from("b") => MonoType::Int(Int),
                },
                opt: HashMap::new(),
                pipe: Some(Property {
                    k: String::from("<-"),
                    v: MonoType::Int(Int),
                }),
                retn: MonoType::Int(Int),
            }
            .to_string()
        );
        assert_eq!(
            "(<-:int, ?a:int, ?b:int) -> int",
            Function {
                req: HashMap::new(),
                opt: maplit::hashmap! {
                    String::from("a") => MonoType::Int(Int),
                    String::from("b") => MonoType::Int(Int),
                },
                pipe: Some(Property {
                    k: String::from("<-"),
                    v: MonoType::Int(Int),
                }),
                retn: MonoType::Int(Int),
            }
            .to_string()
        );
        assert_eq!(
            "(<-:int, a:int, b:int, ?c:int, ?d:int) -> int",
            Function {
                req: maplit::hashmap! {
                    String::from("a") => MonoType::Int(Int),
                    String::from("b") => MonoType::Int(Int),
                },
                opt: maplit::hashmap! {
                    String::from("c") => MonoType::Int(Int),
                    String::from("d") => MonoType::Int(Int),
                },
                pipe: Some(Property {
                    k: String::from("<-"),
                    v: MonoType::Int(Int),
                }),
                retn: MonoType::Int(Int),
            }
            .to_string()
        );
        assert_eq!(
            "(a:int, ?b:bool) -> int",
            Function {
                req: maplit::hashmap! {
                    String::from("a") => MonoType::Int(Int),
                },
                opt: maplit::hashmap! {
                    String::from("b") => MonoType::Bool(Bool),
                },
                pipe: None,
                retn: MonoType::Int(Int),
            }
            .to_string()
        );
        assert_eq!(
            "(<-a:int, b:int, c:int, ?d:bool) -> int",
            Function {
                req: maplit::hashmap! {
                    String::from("b") => MonoType::Int(Int),
                    String::from("c") => MonoType::Int(Int),
                },
                opt: maplit::hashmap! {
                    String::from("d") => MonoType::Bool(Bool),
                },
                pipe: Some(Property {
                    k: String::from("a"),
                    v: MonoType::Int(Int),
                }),
                retn: MonoType::Int(Int),
            }
            .to_string()
        );
    }

    #[test]
    fn display_polytype() {
        assert_eq!(
            "forall [] int",
            PolyType {
                free: Vec::new(),
                bnds: None,
                expr: MonoType::Int(Int),
            }
            .to_string(),
        );
        assert_eq!(
            "forall [t0] (x:t0) -> t0",
            PolyType {
                free: vec![Tvar(0)],
                bnds: None,
                expr: MonoType::Fun(Box::new(Function {
                    req: maplit::hashmap! {
                        String::from("x") => MonoType::Var(Tvar(0)),
                    },
                    opt: HashMap::new(),
                    pipe: None,
                    retn: MonoType::Var(Tvar(0)),
                })),
            }
            .to_string(),
        );
        assert_eq!(
            "forall [t0, t1] (x:t0, y:t1) -> {x:t0 | y:t1 | {}}",
            PolyType {
                free: vec![Tvar(0), Tvar(1)],
                bnds: None,
                expr: MonoType::Fun(Box::new(Function {
                    req: maplit::hashmap! {
                        String::from("x") => MonoType::Var(Tvar(0)),
                        String::from("y") => MonoType::Var(Tvar(1)),
                    },
                    opt: HashMap::new(),
                    pipe: None,
                    retn: MonoType::Row(Box::new(Row::Extension {
                        head: Property {
                            k: String::from("x"),
                            v: MonoType::Var(Tvar(0)),
                        },
                        tail: MonoType::Row(Box::new(Row::Extension {
                            head: Property {
                                k: String::from("y"),
                                v: MonoType::Var(Tvar(1)),
                            },
                            tail: MonoType::Row(Box::new(Row::Empty)),
                        })),
                    })),
                })),
            }
            .to_string(),
        );
        assert_eq!(
            "forall [t0] where t0:Addable (a:t0, b:t0) -> t0",
            PolyType {
                free: vec![Tvar(0)],
                bnds: Some(Const {
                    kinds: maplit::hashmap! {Tvar(0) => maplit::hashset![Kind::Addable]}
                }),
                expr: MonoType::Fun(Box::new(Function {
                    req: maplit::hashmap! {
                        String::from("a") => MonoType::Var(Tvar(0)),
                        String::from("b") => MonoType::Var(Tvar(0)),
                    },
                    opt: HashMap::new(),
                    pipe: None,
                    retn: MonoType::Var(Tvar(0)),
                })),
            }
            .to_string(),
        );
        assert_eq!(
            "forall [t0, t1] where t0:Addable, t1:Divisible (x:t0, y:t1) -> {x:t0 | y:t1 | {}}",
            PolyType {
                free: vec![Tvar(0), Tvar(1)],
                bnds: Some(Const {
                    kinds: maplit::hashmap! {
                        Tvar(0) => maplit::hashset![Kind::Addable],
                        Tvar(1) => maplit::hashset![Kind::Divisible],
                    }
                }),
                expr: MonoType::Fun(Box::new(Function {
                    req: maplit::hashmap! {
                        String::from("x") => MonoType::Var(Tvar(0)),
                        String::from("y") => MonoType::Var(Tvar(1)),
                    },
                    opt: HashMap::new(),
                    pipe: None,
                    retn: MonoType::Row(Box::new(Row::Extension {
                        head: Property {
                            k: String::from("x"),
                            v: MonoType::Var(Tvar(0)),
                        },
                        tail: MonoType::Row(Box::new(Row::Extension {
                            head: Property {
                                k: String::from("y"),
                                v: MonoType::Var(Tvar(1)),
                            },
                            tail: MonoType::Row(Box::new(Row::Empty)),
                        })),
                    })),
                })),
            }
            .to_string(),
        );
        assert_eq!(
            "forall [t0, t1] where t0:Comparable + Equatable, t1:Addable + Divisible (x:t0, y:t1) -> {x:t0 | y:t1 | {}}",
            PolyType {
                free: vec![Tvar(0), Tvar(1)],
                bnds: Some(Const {
                    kinds: maplit::hashmap! {
                        Tvar(0) => maplit::hashset![Kind::Comparable, Kind::Equatable],
                        Tvar(1) => maplit::hashset![Kind::Addable, Kind::Divisible],
                    }
                }),
                expr: MonoType::Fun(Box::new(Function {
                    req: maplit::hashmap! {
                        String::from("x") => MonoType::Var(Tvar(0)),
                        String::from("y") => MonoType::Var(Tvar(1)),
                    },
                    opt: HashMap::new(),
                    pipe: None,
                    retn: MonoType::Row(Box::new(Row::Extension {
                        head: Property {
                            k: String::from("x"),
                            v: MonoType::Var(Tvar(0)),
                        },
                        tail: MonoType::Row(Box::new(Row::Extension {
                            head: Property {
                                k: String::from("y"),
                                v: MonoType::Var(Tvar(1)),
                            },
                            tail: MonoType::Row(Box::new(Row::Empty)),
                        })),
                    })),
                })),
            }
            .to_string(),
        );
    }

    #[test]
    // Ensure any two permutations of the same record are equal
    fn compare_records() {
        assert_eq!(
            // {a:int | b:string | t0}
            MonoType::Row(Box::new(Row::Extension {
                head: Property {
                    k: String::from("a"),
                    v: MonoType::Int(Int),
                },
                tail: MonoType::Row(Box::new(Row::Extension {
                    head: Property {
                        k: String::from("b"),
                        v: MonoType::String(Str),
                    },
                    tail: MonoType::Var(Tvar(0)),
                })),
            })),
            // {b:string | a:int | t0}
            MonoType::Row(Box::new(Row::Extension {
                head: Property {
                    k: String::from("b"),
                    v: MonoType::String(Str),
                },
                tail: MonoType::Row(Box::new(Row::Extension {
                    head: Property {
                        k: String::from("a"),
                        v: MonoType::Int(Int),
                    },
                    tail: MonoType::Var(Tvar(0)),
                })),
            })),
        );
        assert_ne!(
            // {a:int | b:string | {}}
            MonoType::Row(Box::new(Row::Extension {
                head: Property {
                    k: String::from("a"),
                    v: MonoType::Int(Int),
                },
                tail: MonoType::Row(Box::new(Row::Extension {
                    head: Property {
                        k: String::from("b"),
                        v: MonoType::String(Str),
                    },
                    tail: MonoType::Row(Box::new(Row::Empty)),
                })),
            })),
            // {b:int | a:int | {}}
            MonoType::Row(Box::new(Row::Extension {
                head: Property {
                    k: String::from("b"),
                    v: MonoType::Int(Int),
                },
                tail: MonoType::Row(Box::new(Row::Extension {
                    head: Property {
                        k: String::from("a"),
                        v: MonoType::Int(Int),
                    },
                    tail: MonoType::Row(Box::new(Row::Empty)),
                })),
            })),
        );
    }
}
