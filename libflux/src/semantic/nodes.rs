// Module nodes contains the nodes for the semantic graph.
//
// NOTE(affo): At this stage the nodes are a clone of the AST nodes with some type information added.
//  Nevertheless, new node types allow us to decouple this step of compilation from the parsing.
//  This is of paramount importance if we decide to add responsibilities to the semantic analysis and
//  change it independently from the parsing bits.
//  Uncommented node types are a direct port of the AST ones.
//
// NOTE(affo): With respect to the Go implementation, the Extern* nodes have been eliminated,
//  because they where used only for including type inference information in the semantic nodes.
//  We now implement a different strategy here, where type information is embedded in the Expressions.
//
// NOTE(affo): With respect to the Go implementation, the FunctionExpression node has beeen simplified
//  to provide piped arguments and defaults via methods.

extern crate chrono;
use crate::ast;
use crate::semantic::types;

use chrono::prelude::DateTime;
use chrono::Duration;
use chrono::FixedOffset;
use std::vec::Vec;

pub type Location<'a> = &'a ast::SourceLocation;

// TypeInformation represents the type information for a semantic node.
#[derive(Debug, PartialEq, Clone)]
pub struct TypeInformation {
    pub mono_type: types::MonoType,
}

impl TypeInformation {
    pub fn update(&mut self, t: types::MonoType) {
        self.mono_type = t;
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<'a> {
    Expr(ExpressionStatement<'a>),
    Var(VariableAssignment<'a>),
    Opt(OptionStatement<'a>),
    Ret(ReturnStatement<'a>),
    Test(TestStatement<'a>),
    Built(BuiltinStatement<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Assignment<'a> {
    Variable(VariableAssignment<'a>),
    Member(MemberAssignment<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression<'a> {
    Idt(IdentifierExpression<'a>),
    Arr(Box<ArrayExpression<'a>>),
    Fun(Box<FunctionExpression<'a>>),
    Log(Box<LogicalExpression<'a>>),
    Obj(Box<ObjectExpression<'a>>),
    Mem(Box<MemberExpression<'a>>),
    Idx(Box<IndexExpression<'a>>),
    Bin(Box<BinaryExpression<'a>>),
    Un(Box<UnaryExpression<'a>>),
    Call(Box<CallExpression<'a>>),
    Cond(Box<ConditionalExpression<'a>>),
    StringExp(Box<StringExpression<'a>>),

    Int(IntegerLiteral<'a>),
    Flt(FloatLiteral<'a>),
    Str(StringLiteral<'a>),
    Dur(DurationLiteral<'a>),
    Uint(UnsignedIntegerLiteral<'a>),
    Bool(BooleanLiteral<'a>),
    Time(DateTimeLiteral<'a>),
    Regexp(RegexpLiteral<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum PropertyKey<'a> {
    Identifier(Identifier<'a>),
    StringLiteral(StringLiteral<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum FunctionBody<'a> {
    Block(Block<'a>),
    Expr(Expression<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Package<'a> {
    pub loc: Location<'a>,

    pub package: String,
    pub files: Vec<File<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct File<'a> {
    pub loc: Location<'a>,

    pub package: Option<PackageClause<'a>>,
    pub imports: Vec<ImportDeclaration<'a>>,
    pub body: Vec<Statement<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct PackageClause<'a> {
    pub loc: Location<'a>,

    pub name: Identifier<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ImportDeclaration<'a> {
    pub loc: Location<'a>,

    pub alias: Option<Identifier<'a>>,
    pub path: StringLiteral<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block<'a> {
    pub loc: Location<'a>,

    pub body: Vec<Statement<'a>>,
}

impl Block<'_> {
    fn return_statement(&self) -> &ReturnStatement {
        let len = self.body.len();
        let last = self
            .body
            .get(len - 1)
            .expect("body must have at least one statement");
        let last: Option<&ReturnStatement> = match last {
            Statement::Ret(rs) => Some(rs),
            _ => None,
        };
        last.expect("last statement must be a return statement")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct OptionStatement<'a> {
    pub loc: Location<'a>,

    pub assignment: Assignment<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BuiltinStatement<'a> {
    pub loc: Location<'a>,

    pub id: Identifier<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TestStatement<'a> {
    pub loc: Location<'a>,

    pub assignment: VariableAssignment<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExpressionStatement<'a> {
    pub loc: Location<'a>,

    pub expression: Expression<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStatement<'a> {
    pub loc: Location<'a>,

    pub argument: Expression<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct VariableAssignment<'a> {
    pub loc: Location<'a>,

    pub id: Identifier<'a>,
    pub init: Expression<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct MemberAssignment<'a> {
    pub loc: Location<'a>,

    pub member: MemberExpression<'a>,
    pub init: Expression<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StringExpression<'a> {
    pub loc: Location<'a>,
    pub type_info: TypeInformation,

    pub parts: Vec<StringExpressionPart<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum StringExpressionPart<'a> {
    Text(TextPart<'a>),
    Expr(InterpolatedPart<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct TextPart<'a> {
    pub loc: Location<'a>,

    pub value: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct InterpolatedPart<'a> {
    pub loc: Location<'a>,

    pub expression: Expression<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ArrayExpression<'a> {
    pub loc: Location<'a>,
    pub type_info: TypeInformation,

    pub elements: Vec<Expression<'a>>,
}

// FunctionExpression represents the definition of a function
#[derive(Debug, PartialEq, Clone)]
pub struct FunctionExpression<'a> {
    pub loc: Location<'a>,
    pub type_info: TypeInformation,

    pub params: Vec<FunctionParameter<'a>>,
    pub body: FunctionBody<'a>,
}

impl<'a> FunctionExpression<'a> {
    pub fn pipe(&self) -> Option<&FunctionParameter<'a>> {
        for p in &self.params {
            if p.is_pipe {
                return Some(p);
            }
        }
        None
    }

    pub fn defaults(&self) -> Vec<&FunctionParameter<'a>> {
        let mut ds = Vec::new();
        for p in &self.params {
            match p.default {
                Some(_) => ds.push(p),
                None => (),
            }
        }
        ds
    }
}

// FunctionParameter represents a function parameter.
#[derive(Debug, PartialEq, Clone)]
pub struct FunctionParameter<'a> {
    pub loc: Location<'a>,

    pub is_pipe: bool,
    pub key: Identifier<'a>,
    pub default: Option<Expression<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BinaryExpression<'a> {
    pub loc: Location<'a>,
    pub type_info: TypeInformation,

    pub operator: ast::OperatorKind,
    pub left: Expression<'a>,
    pub right: Expression<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallExpression<'a> {
    pub loc: Location<'a>,
    pub type_info: TypeInformation,

    pub callee: Expression<'a>,
    pub arguments: ObjectExpression<'a>,
    pub pipe: Option<Expression<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ConditionalExpression<'a> {
    pub loc: Location<'a>,
    pub type_info: TypeInformation,

    pub test: Expression<'a>,
    pub consequent: Expression<'a>,
    pub alternate: Expression<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LogicalExpression<'a> {
    pub loc: Location<'a>,
    pub type_info: TypeInformation,

    pub operator: ast::LogicalOperatorKind,
    pub left: Expression<'a>,
    pub right: Expression<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct MemberExpression<'a> {
    pub loc: Location<'a>,
    pub type_info: TypeInformation,

    pub object: Expression<'a>,
    pub property: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IndexExpression<'a> {
    pub loc: Location<'a>,
    pub type_info: TypeInformation,

    pub array: Expression<'a>,
    pub index: Expression<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ObjectExpression<'a> {
    pub loc: Location<'a>,
    pub type_info: TypeInformation,

    pub with: Option<IdentifierExpression<'a>>,
    pub properties: Vec<Property<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnaryExpression<'a> {
    pub loc: Location<'a>,
    pub type_info: TypeInformation,

    pub operator: ast::OperatorKind,
    pub argument: Expression<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Property<'a> {
    pub loc: Location<'a>,

    pub key: PropertyKey<'a>,
    pub value: Option<Expression<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IdentifierExpression<'a> {
    pub loc: Location<'a>,
    pub type_info: TypeInformation,

    pub name: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier<'a> {
    pub loc: Location<'a>,

    pub name: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BooleanLiteral<'a> {
    pub loc: Location<'a>,
    pub type_info: TypeInformation,

    pub value: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IntegerLiteral<'a> {
    pub loc: Location<'a>,
    pub type_info: TypeInformation,

    pub value: i64,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FloatLiteral<'a> {
    pub loc: Location<'a>,
    pub type_info: TypeInformation,

    pub value: f64,
}

#[derive(Debug, PartialEq, Clone)]
pub struct RegexpLiteral<'a> {
    pub loc: Location<'a>,
    pub type_info: TypeInformation,

    // TODO(affo): should this be a compiled regexp?
    pub value: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StringLiteral<'a> {
    pub loc: Location<'a>,
    pub type_info: TypeInformation,

    pub value: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnsignedIntegerLiteral<'a> {
    pub loc: Location<'a>,
    pub type_info: TypeInformation,

    pub value: u64,
}

#[derive(Debug, PartialEq, Clone)]
pub struct DateTimeLiteral<'a> {
    pub loc: Location<'a>,
    pub type_info: TypeInformation,

    pub value: DateTime<FixedOffset>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct DurationLiteral<'a> {
    pub loc: Location<'a>,
    pub type_info: TypeInformation,

    pub value: Duration,
}

const NANOS: i64 = 1;
const MICROS: i64 = NANOS * 1000;
const MILLIS: i64 = MICROS * 1000;
const SECONDS: i64 = MILLIS * 1000;
const MINUTES: i64 = SECONDS * 60;
const HOURS: i64 = MINUTES * 60;
const DAYS: i64 = HOURS * 24;
const WEEKS: i64 = DAYS * 7;
const MONTHS: f64 = WEEKS as f64 * (365.25 / 12.0 / 7.0);
const YEARS: f64 = MONTHS * 12.0;

// TODO(affo): this is not accurate, a duration value depends on the time in which it is calculated.
// 1 month is different if now is the 1st of January, or the 1st of February.
// Some days do not last 24 hours because of light savings.
pub fn convert_duration(duration: &Vec<ast::Duration>) -> Result<Duration, String> {
    let d = duration
        .iter()
        .try_fold(0 as i64, |acc, d| match d.unit.as_str() {
            "y" => Ok(acc + (d.magnitude as f64 * YEARS) as i64),
            "mo" => Ok(acc + (d.magnitude as f64 * MONTHS) as i64),
            "w" => Ok(acc + d.magnitude * WEEKS),
            "d" => Ok(acc + d.magnitude * DAYS),
            "h" => Ok(acc + d.magnitude * HOURS),
            "m" => Ok(acc + d.magnitude * MINUTES),
            "s" => Ok(acc + d.magnitude * SECONDS),
            "ms" => Ok(acc + d.magnitude * MILLIS),
            "us" | "µs" => Ok(acc + d.magnitude * MICROS),
            "ns" => Ok(acc + d.magnitude * NANOS),
            _ => Err(format!("unrecognized magnitude for duration: {}", d.unit)),
        })?;
    Ok(Duration::nanoseconds(d))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn duration_conversion_ok() {
        let t = vec![
            ast::Duration {
                magnitude: 1,
                unit: "y".to_string(),
            },
            ast::Duration {
                magnitude: 2,
                unit: "mo".to_string(),
            },
            ast::Duration {
                magnitude: 3,
                unit: "w".to_string(),
            },
            ast::Duration {
                magnitude: 4,
                unit: "m".to_string(),
            },
            ast::Duration {
                magnitude: 5,
                unit: "ns".to_string(),
            },
        ];
        let exp = (1.0 * YEARS + 2.0 * MONTHS) as i64 + 3 * WEEKS + 4 * MINUTES + 5 * NANOS;
        let got = convert_duration(&t).unwrap();
        assert_eq!(exp, got.num_nanoseconds().expect("should not overflow"));
    }

    #[test]
    fn duration_conversion_doubled_magnitude() {
        let t = vec![
            ast::Duration {
                magnitude: 1,
                unit: "y".to_string(),
            },
            ast::Duration {
                magnitude: 2,
                unit: "mo".to_string(),
            },
            ast::Duration {
                magnitude: 3,
                unit: "y".to_string(),
            },
        ];
        let exp = (4.0 * YEARS + 2.0 * MONTHS) as i64;
        let got = convert_duration(&t).unwrap();
        assert_eq!(exp, got.num_nanoseconds().expect("should not overflow"));
    }

    #[test]
    fn duration_conversion_negative() {
        let t = vec![
            ast::Duration {
                magnitude: -1,
                unit: "y".to_string(),
            },
            ast::Duration {
                magnitude: 2,
                unit: "mo".to_string(),
            },
            ast::Duration {
                magnitude: -3,
                unit: "w".to_string(),
            },
        ];
        let exp = (-1.0 * YEARS + 2.0 * MONTHS) as i64 - 3 * WEEKS;
        let got = convert_duration(&t).unwrap();
        assert_eq!(exp, got.num_nanoseconds().expect("should not overflow"));
    }

    #[test]
    fn duration_conversion_error() {
        let t = vec![
            ast::Duration {
                magnitude: -1,
                unit: "y".to_string(),
            },
            ast::Duration {
                magnitude: 2,
                unit: "--idk--".to_string(),
            },
            ast::Duration {
                magnitude: -3,
                unit: "w".to_string(),
            },
        ];
        let exp = "unrecognized magnitude for duration: --idk--";
        let got = convert_duration(&t).err().expect("should be an error");
        assert_eq!(exp, got.to_string());
    }
}
