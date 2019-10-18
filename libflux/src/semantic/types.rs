use std::{cmp, collections::HashMap, fmt};

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
    pub bnds: Option<HashMap<Tvar, Kind>>,
    pub expr: MonoType,
}

impl fmt::Display for PolyType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.bnds {
            Some(bnds) => {
                let mut bounds = Vec::new();
                for tv in &self.free {
                    if let Some(kind) = bnds.get(tv) {
                        bounds.push(BoundTvar {
                            tv: *tv,
                            kind: *kind,
                        })
                    }
                }
                write!(
                    f,
                    "forall [{}] where {} {}",
                    DisplayList {
                        values: &self.free,
                        delim: ", "
                    },
                    DisplayList {
                        values: &bounds,
                        delim: ", "
                    },
                    self.expr,
                )
            }
            None => write!(
                f,
                "forall [{}] {}",
                DisplayList {
                    values: &self.free,
                    delim: ", "
                },
                self.expr
            ),
        }
    }
}

// Kind represents a class or family of types
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Kind {
    Addable,
    Subtractable,
    Divisible,
    Comparable,
    Nullable,
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Kind::Addable => f.write_str("Addable"),
            Kind::Subtractable => f.write_str("Subtractable"),
            Kind::Divisible => f.write_str("Divisible"),
            Kind::Comparable => f.write_str("Comparable"),
            Kind::Nullable => f.write_str("Nullable"),
        }
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Bool;

impl fmt::Display for Bool {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("bool")
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Int;

impl fmt::Display for Int {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("int")
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Uint;

impl fmt::Display for Uint {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("uint")
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Float;

impl fmt::Display for Float {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("float")
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Str;

impl fmt::Display for Str {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("string")
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Duration;

impl fmt::Display for Duration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("duration")
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Time;

impl fmt::Display for Time {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("time")
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Regexp;

impl fmt::Display for Regexp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("regexp")
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

// Array is a homogeneous list type
#[derive(Debug, Clone, PartialEq)]
pub struct Array(pub MonoType);

impl fmt::Display for Array {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}]", self.0)
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
        let mut req: Vec<_> = self
            .req
            .iter()
            .map(|(k, v)| Property {
                k: k.to_string(),
                v: v.clone(),
            })
            .collect();

        req.sort_by(|a, b| a.k.cmp(&b.k));

        let mut opt: Vec<_> = self
            .opt
            .iter()
            .map(|(k, v)| Property {
                k: String::from("?") + &k,
                v: v.clone(),
            })
            .collect();

        opt.sort_by(|a, b| a.k.cmp(&b.k));

        let mut args = Vec::with_capacity(req.len() + opt.len());

        if let Some(pipe) = &self.pipe {
            if pipe.k == "<-" {
                args.push(pipe.clone());
            } else {
                args.push(Property {
                    k: String::from("<-") + &pipe.k,
                    v: pipe.v.clone(),
                });
            }
        }

        args.append(&mut req);
        args.append(&mut opt);

        write!(
            f,
            "({}) -> {}",
            DisplayList {
                values: args,
                delim: ", "
            },
            self.retn
        )
    }
}

// BoundTvar represents a constrained type variable.
// Used solely for displaying the generic constraints of a polytype.
#[derive(Debug)]
struct BoundTvar {
    tv: Tvar,
    kind: Kind,
}

impl fmt::Display for BoundTvar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.tv, self.kind)
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
                bnds: Some(maplit::hashmap! {
                    Tvar(0) => Kind::Addable,
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
                bnds: Some(maplit::hashmap! {
                    Tvar(0) => Kind::Addable,
                    Tvar(1) => Kind::Divisible,
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
                            })
                            .extend(Property {
                                k: String::from("x"),
                                v: MonoType::Var(Tvar(0)),
                            })
                    )),
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
