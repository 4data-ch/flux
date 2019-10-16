use crate::ast;
use crate::semantic::nodes::*;
use crate::semantic::types::{Fresher, IncrementingFresher, MonoType};
use std::result;

type SemanticError = String;
type Result<T> = result::Result<T, SemanticError>;

pub fn analyze(pkg: &ast::Package) -> Result<Package> {
    analyze_package(pkg, &mut IncrementingFresher::new())
    // TODO(affo): run checks on the semantic graph.
}

fn type_info(fresher: &mut Fresher) -> TypeInformation {
    TypeInformation {
        mono_type: MonoType::Var(fresher.get()),
    }
}

fn analyze_package<'a>(pkg: &'a ast::Package, fresher: &mut Fresher) -> Result<Package<'a>> {
    let files = pkg
        .files
        .iter()
        .map(|f| analyze_file(f, fresher))
        .collect::<Result<Vec<File>>>()?;
    Ok(Package {
        loc: &pkg.base.location,
        package: pkg.package.clone(),
        files: files,
    })
}

fn analyze_file<'a>(file: &'a ast::File, fresher: &mut Fresher) -> Result<File<'a>> {
    let package = analyze_package_clause(&file.package, fresher)?;
    let imports = file
        .imports
        .iter()
        .map(|i| analyze_import_declaration(i, fresher))
        .collect::<Result<Vec<ImportDeclaration>>>()?;
    let body = file
        .body
        .iter()
        .map(|s| analyze_statement(s, fresher))
        .collect::<Result<Vec<Statement>>>()?;
    Ok(File {
        loc: &file.base.location,
        package,
        imports,
        body,
    })
}

fn analyze_package_clause<'a>(
    pkg: &'a Option<ast::PackageClause>,
    fresher: &mut Fresher,
) -> Result<Option<PackageClause<'a>>> {
    if pkg.is_none() {
        return Ok(None);
    }
    let pkg = pkg.as_ref().unwrap();
    let name = analyze_identifier(&pkg.name, fresher)?;
    Ok(Some(PackageClause {
        loc: &pkg.base.location,
        name,
    }))
}

fn analyze_import_declaration<'a>(
    imp: &'a ast::ImportDeclaration,
    fresher: &mut Fresher,
) -> Result<ImportDeclaration<'a>> {
    let alias = match &imp.alias {
        None => None,
        Some(id) => Some(analyze_identifier(&id, fresher)?),
    };
    let path = analyze_string_literal(&imp.path, fresher)?;
    Ok(ImportDeclaration {
        loc: &imp.base.location,
        alias,
        path,
    })
}

fn analyze_statement<'a>(stmt: &'a ast::Statement, fresher: &mut Fresher) -> Result<Statement<'a>> {
    match &stmt {
        ast::Statement::Opt(s) => Ok(Statement::Opt(analyze_option_statement(&s, fresher)?)),
        ast::Statement::Built(s) => Ok(Statement::Built(analyze_builtin_statement(&s, fresher)?)),
        ast::Statement::Test(s) => Ok(Statement::Test(analyze_test_statement(&s, fresher)?)),
        ast::Statement::Expr(s) => Ok(Statement::Expr(analyze_expression_statement(&s, fresher)?)),
        ast::Statement::Ret(s) => Ok(Statement::Ret(analyze_return_statement(&s, fresher)?)),
        // TODO(affo): we should fix this to include MemberAssignement.
        //  The error lies in AST: the Statement enum does not include that.
        //  This is not a problem when parsing, because we parse it only in the option assignment case,
        //  and we return an OptionStatement, which is a Statement.
        ast::Statement::Var(s) => Ok(Statement::Var(analyze_variable_assignment(&s, fresher)?)),
        // This should not happen, but it could for BadStatement.
        _ => Err("unsupported statement in semantic analysis".to_string()),
    }
}

fn analyze_assignment<'a>(
    assign: &'a ast::Assignment,
    fresher: &mut Fresher,
) -> Result<Assignment<'a>> {
    match &assign {
        ast::Assignment::Variable(a) => Ok(Assignment::Variable(analyze_variable_assignment(
            &a, fresher,
        )?)),
        ast::Assignment::Member(a) => {
            Ok(Assignment::Member(analyze_member_assignment(&a, fresher)?))
        }
    }
}

fn analyze_option_statement<'a>(
    stmt: &'a ast::OptionStatement,
    fresher: &mut Fresher,
) -> Result<OptionStatement<'a>> {
    Ok(OptionStatement {
        loc: &stmt.base.location,
        assignment: analyze_assignment(&stmt.assignment, fresher)?,
    })
}

fn analyze_builtin_statement<'a>(
    stmt: &'a ast::BuiltinStatement,
    fresher: &mut Fresher,
) -> Result<BuiltinStatement<'a>> {
    Ok(BuiltinStatement {
        loc: &stmt.base.location,
        id: analyze_identifier(&stmt.id, fresher)?,
    })
}

fn analyze_test_statement<'a>(
    stmt: &'a ast::TestStatement,
    fresher: &mut Fresher,
) -> Result<TestStatement<'a>> {
    Ok(TestStatement {
        loc: &stmt.base.location,
        assignment: analyze_variable_assignment(&stmt.assignment, fresher)?,
    })
}

fn analyze_expression_statement<'a>(
    stmt: &'a ast::ExpressionStatement,
    fresher: &mut Fresher,
) -> Result<ExpressionStatement<'a>> {
    Ok(ExpressionStatement {
        loc: &stmt.base.location,
        expression: analyze_expression(&stmt.expression, fresher)?,
    })
}

fn analyze_return_statement<'a>(
    stmt: &'a ast::ReturnStatement,
    fresher: &mut Fresher,
) -> Result<ReturnStatement<'a>> {
    Ok(ReturnStatement {
        loc: &stmt.base.location,
        argument: analyze_expression(&stmt.argument, fresher)?,
    })
}

fn analyze_variable_assignment<'a>(
    stmt: &'a ast::VariableAssignment,
    fresher: &mut Fresher,
) -> Result<VariableAssignment<'a>> {
    Ok(VariableAssignment {
        loc: &stmt.base.location,
        id: analyze_identifier(&stmt.id, fresher)?,
        init: analyze_expression(&stmt.init, fresher)?,
    })
}

fn analyze_member_assignment<'a>(
    stmt: &'a ast::MemberAssignment,
    fresher: &mut Fresher,
) -> Result<MemberAssignment<'a>> {
    Ok(MemberAssignment {
        loc: &stmt.base.location,
        member: analyze_member_expression(&stmt.member, fresher)?,
        init: analyze_expression(&stmt.init, fresher)?,
    })
}

fn analyze_expression<'a>(
    expr: &'a ast::Expression,
    fresher: &mut Fresher,
) -> Result<Expression<'a>> {
    match &expr {
        ast::Expression::Fun(expr) => Ok(Expression::Fun(Box::new(analyze_function_expression(&expr, fresher)?))),
        ast::Expression::Call(expr) => Ok(Expression::Call(Box::new(analyze_call_expression(&expr, fresher)?))),
        ast::Expression::Mem(expr) => Ok(Expression::Mem(Box::new(analyze_member_expression(&expr, fresher)?))),
        ast::Expression::Idx(expr) => Ok(Expression::Idx(Box::new(analyze_index_expression(&expr, fresher)?))),
        ast::Expression::Pipe(expr) => Ok(Expression::Call(Box::new(analyze_pipe_expression(&expr, fresher)?))),
        ast::Expression::Bin(expr) => Ok(Expression::Bin(Box::new(analyze_binary_expression(&expr, fresher)?))),
        ast::Expression::Un(expr) => Ok(Expression::Un(Box::new(analyze_unary_expression(&expr, fresher)?))),
        ast::Expression::Log(expr) => Ok(Expression::Log(Box::new(analyze_logical_expression(&expr, fresher)?))),
        ast::Expression::Cond(expr) => Ok(Expression::Cond(Box::new(analyze_conditional_expression(&expr, fresher)?))),
        ast::Expression::Obj(expr) => Ok(Expression::Obj(Box::new(analyze_object_expression(&expr, fresher)?))),
        ast::Expression::Arr(expr) => Ok(Expression::Arr(Box::new(analyze_array_expression(&expr, fresher)?))),
        ast::Expression::Idt(expr) => Ok(Expression::Idt(analyze_identifier_expression(&expr, fresher)?)),
        ast::Expression::StringExp(expr) => Ok(Expression::StringExp(Box::new(analyze_string_expression(&expr, fresher)?))),
        ast::Expression::Paren(expr) => analyze_expression(&expr.expression, fresher),
        ast::Expression::Str(lit) => Ok(Expression::Str(analyze_string_literal(&lit, fresher)?)),
        ast::Expression::Bool(lit) => Ok(Expression::Bool(analyze_boolean_literal(&lit, fresher)?)),
        ast::Expression::Flt(lit) => Ok(Expression::Flt(analyze_float_literal(&lit, fresher)?)),
        ast::Expression::Int(lit) => Ok(Expression::Int(analyze_integer_literal(&lit, fresher)?)),
        ast::Expression::Uint(lit) => Ok(Expression::Uint(analyze_unsigned_integer_literal(&lit, fresher)?)),
        ast::Expression::Regexp(lit) => Ok(Expression::Regexp(analyze_regexp_literal(&lit, fresher)?)),
        ast::Expression::Dur(lit) => Ok(Expression::Dur(analyze_duration_literal(&lit, fresher)?)),
        ast::Expression::Time(lit) => Ok(Expression::Time(analyze_time_literal(&lit, fresher)?)),
        ast::Expression::PipeLit(_) => Err("a pipe literal may only be used as a default value for an argument in a function definition".to_string()),
        // This should not happen, but it could for BadExpression.
        _ => Err("unsupported expression in semantic analysis".to_string())
    }
}

fn analyze_function_expression<'a>(
    expr: &'a ast::FunctionExpression,
    fresher: &mut Fresher,
) -> Result<FunctionExpression<'a>> {
    let params = analyze_function_params(&expr.params, fresher)?;
    let body = analyze_function_body(&expr.body, fresher)?;
    Ok(FunctionExpression {
        loc: &expr.base.location,
        type_info: type_info(fresher),
        params,
        body,
    })
}

fn analyze_function_params<'a>(
    props: &'a Vec<ast::Property>,
    fresher: &mut Fresher,
) -> Result<Vec<FunctionParameter<'a>>> {
    // The iteration here is complex, cannot use iter().map()..., better to write it explicitly.
    let mut params: Vec<FunctionParameter> = Vec::new();
    let mut piped = false;
    for prop in props {
        let id = match &prop.key {
            ast::PropertyKey::Identifier(id) => Ok(id),
            _ => Err("function params must be identifiers".to_string()),
        }?;
        let key = analyze_identifier(&id, fresher)?;
        let mut default: Option<Expression> = None;
        let mut is_pipe = false;
        match &prop.value {
            Some(expr) => match expr {
                ast::Expression::PipeLit(_) => {
                    if piped {
                        return Err("only a single argument may be piped".to_string());
                    } else {
                        piped = true;
                        is_pipe = true;
                    };
                }
                e => default = Some(analyze_expression(&e, fresher)?),
            },
            None => (),
        };
        params.push(FunctionParameter {
            loc: &prop.base.location,
            is_pipe,
            key,
            default,
        });
    }
    Ok(params)
}

fn analyze_function_body<'a>(
    body: &'a ast::FunctionBody,
    fresher: &mut Fresher,
) -> Result<FunctionBody<'a>> {
    match &body {
        ast::FunctionBody::Expr(expr) => {
            Ok(FunctionBody::Expr(analyze_expression(&expr, fresher)?))
        }
        ast::FunctionBody::Block(block) => Ok(FunctionBody::Block(analyze_block(&block, fresher)?)),
    }
}

fn analyze_block<'a>(block: &'a ast::Block, fresher: &mut Fresher) -> Result<Block<'a>> {
    let stmts = block
        .body
        .iter()
        .map(|s| analyze_statement(s, fresher))
        .collect::<Result<Vec<Statement>>>()?;
    match &stmts.get(stmts.len() - 1) {
        Some(Statement::Ret(_)) => Ok(Block {
            loc: &block.base.location,
            body: stmts,
        }),
        _ => Err("missing return statement in block".to_string()),
    }
}

fn analyze_call_expression<'a>(
    expr: &'a ast::CallExpression,
    fresher: &mut Fresher,
) -> Result<CallExpression<'a>> {
    let callee = analyze_expression(&expr.callee, fresher)?;
    if expr.arguments.len() > 1 {
        return Err("arguments are not a single object expression".to_string());
    }

    let mut args = expr
        .arguments
        .iter()
        .map(|a| match a {
            ast::Expression::Obj(obj) => analyze_object_expression(&obj, fresher),
            _ => Err("arguments not an object expression".to_string()),
        })
        .collect::<Result<Vec<ObjectExpression>>>()?;

    let loc = &expr.base.location;
    let arguments = match args.len() {
        0 => Ok(ObjectExpression {
            loc,
            type_info: type_info(fresher),
            with: None,
            properties: Vec::new(),
        }),
        1 => Ok(args.pop().expect("there must be 1 element")),
        _ => Err("arguments are more than one object expression".to_string()),
    }?;
    Ok(CallExpression {
        loc,
        type_info: type_info(fresher),
        callee,
        arguments,
        pipe: None,
    })
}

fn analyze_member_expression<'a>(
    expr: &'a ast::MemberExpression,
    fresher: &mut Fresher,
) -> Result<MemberExpression<'a>> {
    let object = analyze_expression(&expr.object, fresher)?;
    let property = match &expr.property {
        ast::PropertyKey::Identifier(id) => id.name.clone(),
        ast::PropertyKey::StringLiteral(lit) => lit.value.clone(),
    };
    Ok(MemberExpression {
        loc: &expr.base.location,
        type_info: type_info(fresher),
        object,
        property,
    })
}

fn analyze_index_expression<'a>(
    expr: &'a ast::IndexExpression,
    fresher: &mut Fresher,
) -> Result<IndexExpression<'a>> {
    let array = analyze_expression(&expr.array, fresher)?;
    let index = analyze_expression(&expr.index, fresher)?;
    Ok(IndexExpression {
        loc: &expr.base.location,
        type_info: type_info(fresher),
        array,
        index,
    })
}

fn analyze_pipe_expression<'a>(
    expr: &'a ast::PipeExpression,
    fresher: &mut Fresher,
) -> Result<CallExpression<'a>> {
    let mut call = analyze_call_expression(&expr.call, fresher)?;
    let pipe = analyze_expression(&expr.argument, fresher)?;
    call.pipe = Some(pipe);
    Ok(call)
}

fn analyze_binary_expression<'a>(
    expr: &'a ast::BinaryExpression,
    fresher: &mut Fresher,
) -> Result<BinaryExpression<'a>> {
    let left = analyze_expression(&expr.left, fresher)?;
    let right = analyze_expression(&expr.right, fresher)?;
    Ok(BinaryExpression {
        loc: &expr.base.location,
        type_info: type_info(fresher),
        operator: expr.operator.clone(),
        left,
        right,
    })
}

fn analyze_unary_expression<'a>(
    expr: &'a ast::UnaryExpression,
    fresher: &mut Fresher,
) -> Result<UnaryExpression<'a>> {
    let argument = analyze_expression(&expr.argument, fresher)?;
    Ok(UnaryExpression {
        loc: &expr.base.location,
        type_info: type_info(fresher),
        operator: expr.operator.clone(),
        argument,
    })
}

fn analyze_logical_expression<'a>(
    expr: &'a ast::LogicalExpression,
    fresher: &mut Fresher,
) -> Result<LogicalExpression<'a>> {
    let left = analyze_expression(&expr.left, fresher)?;
    let right = analyze_expression(&expr.right, fresher)?;
    Ok(LogicalExpression {
        loc: &expr.base.location,
        type_info: type_info(fresher),
        operator: expr.operator.clone(),
        left,
        right,
    })
}

fn analyze_conditional_expression<'a>(
    expr: &'a ast::ConditionalExpression,
    fresher: &mut Fresher,
) -> Result<ConditionalExpression<'a>> {
    let test = analyze_expression(&expr.test, fresher)?;
    let consequent = analyze_expression(&expr.consequent, fresher)?;
    let alternate = analyze_expression(&expr.alternate, fresher)?;
    Ok(ConditionalExpression {
        loc: &expr.base.location,
        type_info: type_info(fresher),
        test,
        consequent,
        alternate,
    })
}

fn analyze_object_expression<'a>(
    expr: &'a ast::ObjectExpression,
    fresher: &mut Fresher,
) -> Result<ObjectExpression<'a>> {
    let properties = expr
        .properties
        .iter()
        .map(|p| analyze_property(p, fresher))
        .collect::<Result<Vec<Property>>>()?;
    let with = match &expr.with {
        Some(id) => Some(analyze_identifier_expression(id, fresher)?),
        None => None,
    };
    Ok(ObjectExpression {
        loc: &expr.base.location,
        type_info: type_info(fresher),
        with,
        properties,
    })
}

fn analyze_property<'a>(prop: &'a ast::Property, fresher: &mut Fresher) -> Result<Property<'a>> {
    let loc: Location;
    let mut name: String;
    let key = match &prop.key {
        ast::PropertyKey::Identifier(id) => {
            let id = analyze_identifier(&id, fresher)?;
            loc = id.loc;
            name = id.name.clone();
            PropertyKey::Identifier(id)
        }
        ast::PropertyKey::StringLiteral(lit) => {
            let lit = analyze_string_literal(&lit, fresher)?;
            loc = lit.loc;
            name = lit.value.clone();
            PropertyKey::StringLiteral(lit)
        }
    };
    let value = Some(match &prop.value {
        Some(expr) => analyze_expression(&expr, fresher)?,
        None => Expression::Idt(IdentifierExpression {
            loc,
            type_info: type_info(fresher),
            name,
        }),
    });
    Ok(Property {
        loc: &prop.base.location,
        key,
        value,
    })
}

fn analyze_array_expression<'a>(
    expr: &'a ast::ArrayExpression,
    fresher: &mut Fresher,
) -> Result<ArrayExpression<'a>> {
    let elements = expr
        .elements
        .iter()
        .map(|e| analyze_expression(e, fresher))
        .collect::<Result<Vec<Expression>>>()?;
    Ok(ArrayExpression {
        loc: &expr.base.location,
        type_info: type_info(fresher),
        elements,
    })
}

fn analyze_identifier<'a>(
    id: &'a ast::Identifier,
    _fresher: &mut Fresher,
) -> Result<Identifier<'a>> {
    Ok(Identifier {
        loc: &id.base.location,
        name: id.name.clone(),
    })
}

fn analyze_identifier_expression<'a>(
    id: &'a ast::Identifier,
    fresher: &mut Fresher,
) -> Result<IdentifierExpression<'a>> {
    Ok(IdentifierExpression {
        loc: &id.base.location,
        type_info: type_info(fresher),
        name: id.name.clone(),
    })
}

fn analyze_string_expression<'a>(
    expr: &'a ast::StringExpression,
    fresher: &mut Fresher,
) -> Result<StringExpression<'a>> {
    let parts = expr
        .parts
        .iter()
        .map(|p| analyze_string_expression_part(p, fresher))
        .collect::<Result<Vec<StringExpressionPart>>>()?;
    Ok(StringExpression {
        loc: &expr.base.location,
        type_info: type_info(fresher),
        parts,
    })
}

fn analyze_string_expression_part<'a>(
    expr: &'a ast::StringExpressionPart,
    fresher: &mut Fresher,
) -> Result<StringExpressionPart<'a>> {
    match &expr {
        ast::StringExpressionPart::Text(txt) => Ok(StringExpressionPart::Text(TextPart {
            loc: &txt.base.location,
            value: txt.value.clone(),
        })),
        ast::StringExpressionPart::Expr(itp) => Ok(StringExpressionPart::Expr(InterpolatedPart {
            loc: &itp.base.location,
            expression: analyze_expression(&itp.expression, fresher)?,
        })),
    }
}

fn analyze_string_literal<'a>(
    lit: &'a ast::StringLiteral,
    fresher: &mut Fresher,
) -> Result<StringLiteral<'a>> {
    Ok(StringLiteral {
        loc: &lit.base.location,
        type_info: type_info(fresher),
        value: lit.value.clone(),
    })
}

fn analyze_boolean_literal<'a>(
    lit: &'a ast::BooleanLiteral,
    fresher: &mut Fresher,
) -> Result<BooleanLiteral<'a>> {
    Ok(BooleanLiteral {
        loc: &lit.base.location,
        type_info: type_info(fresher),
        value: lit.value,
    })
}

fn analyze_float_literal<'a>(
    lit: &'a ast::FloatLiteral,
    fresher: &mut Fresher,
) -> Result<FloatLiteral<'a>> {
    Ok(FloatLiteral {
        loc: &lit.base.location,
        type_info: type_info(fresher),
        value: lit.value,
    })
}

fn analyze_integer_literal<'a>(
    lit: &'a ast::IntegerLiteral,
    fresher: &mut Fresher,
) -> Result<IntegerLiteral<'a>> {
    Ok(IntegerLiteral {
        loc: &lit.base.location,
        type_info: type_info(fresher),
        value: lit.value,
    })
}

fn analyze_unsigned_integer_literal<'a>(
    lit: &'a ast::UnsignedIntegerLiteral,
    fresher: &mut Fresher,
) -> Result<UnsignedIntegerLiteral<'a>> {
    Ok(UnsignedIntegerLiteral {
        loc: &lit.base.location,
        type_info: type_info(fresher),
        value: lit.value,
    })
}

fn analyze_regexp_literal<'a>(
    lit: &'a ast::RegexpLiteral,
    fresher: &mut Fresher,
) -> Result<RegexpLiteral<'a>> {
    Ok(RegexpLiteral {
        loc: &lit.base.location,
        type_info: type_info(fresher),
        value: lit.value.clone(),
    })
}

fn analyze_duration_literal<'a>(
    lit: &'a ast::DurationLiteral,
    fresher: &mut Fresher,
) -> Result<DurationLiteral<'a>> {
    Ok(DurationLiteral {
        loc: &lit.base.location,
        type_info: type_info(fresher),
        value: convert_duration(&lit.values)?,
    })
}

fn analyze_time_literal<'a>(
    lit: &'a ast::DateTimeLiteral,
    fresher: &mut Fresher,
) -> Result<DateTimeLiteral<'a>> {
    Ok(DateTimeLiteral {
        loc: &lit.base.location,
        type_info: type_info(fresher),
        value: lit.value,
    })
}

// In these tests we test the results of semantic analysis on some ASTs.
// NOTE: we do not care about locations.
// We create a default base node and clone it in various AST nodes to make the borrow checker happy.
// We then use a pointer to its location in semantic nodes.
// This works because standard equality between references checks the content of the pointers and not
// the value of the pointers themselves.
#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::types::{Fresher, Tvar};
    use pretty_assertions::assert_eq;

    // TestFresher mocks the fresher so that we don't have to test Tvar ids.
    // That is not straightforward because the IncrementingFresher is used at different depths in
    // the semantic graph.
    struct TestFresher {}

    impl Fresher for TestFresher {
        fn get(&mut self) -> Tvar {
            Tvar(0)
        }
    }

    #[test]
    fn test_analyze_empty() {
        let b = ast::BaseNode::default();
        let pkg = ast::Package {
            base: b.clone(),
            path: "path".to_string(),
            package: "main".to_string(),
            files: Vec::new(),
        };
        let want = Package {
            loc: &b.location,
            package: "main".to_string(),
            files: Vec::new(),
        };
        let got = analyze(&pkg).unwrap();
        assert_eq!(want, got);
    }

    #[test]
    fn test_analyze_package() {
        let b = ast::BaseNode::default();
        let pkg = ast::Package {
            base: b.clone(),
            path: "path".to_string(),
            package: "main".to_string(),
            files: vec![ast::File {
                base: b.clone(),
                name: "foo.flux".to_string(),
                package: Some(ast::PackageClause {
                    base: b.clone(),
                    name: ast::Identifier {
                        base: b.clone(),
                        name: "foo".to_string(),
                    },
                }),
                imports: Vec::new(),
                body: Vec::new(),
            }],
        };
        let want = Package {
            loc: &b.location,
            package: "main".to_string(),
            files: vec![File {
                loc: &b.location,
                package: Some(PackageClause {
                    loc: &b.location,
                    name: Identifier {
                        loc: &b.location,
                        name: "foo".to_string(),
                    },
                }),
                imports: Vec::new(),
                body: Vec::new(),
            }],
        };
        let got = analyze(&pkg).unwrap();
        assert_eq!(want, got);
    }

    #[test]
    fn test_analyze_imports() {
        let b = ast::BaseNode::default();
        let fresher = &mut TestFresher {};
        let pkg = ast::Package {
            base: b.clone(),
            path: "path".to_string(),
            package: "main".to_string(),
            files: vec![ast::File {
                base: b.clone(),
                name: "foo.flux".to_string(),
                package: Some(ast::PackageClause {
                    base: b.clone(),
                    name: ast::Identifier {
                        base: b.clone(),
                        name: "foo".to_string(),
                    },
                }),
                imports: vec![
                    ast::ImportDeclaration {
                        base: b.clone(),
                        path: ast::StringLiteral {
                            base: b.clone(),
                            value: "path/foo".to_string(),
                        },
                        alias: None,
                    },
                    ast::ImportDeclaration {
                        base: b.clone(),
                        path: ast::StringLiteral {
                            base: b.clone(),
                            value: "path/bar".to_string(),
                        },
                        alias: Some(ast::Identifier {
                            base: b.clone(),
                            name: "b".to_string(),
                        }),
                    },
                ],
                body: Vec::new(),
            }],
        };
        let want = Package {
            loc: &b.location,
            package: "main".to_string(),
            files: vec![File {
                loc: &b.location,
                package: Some(PackageClause {
                    loc: &b.location,
                    name: Identifier {
                        loc: &b.location,
                        name: "foo".to_string(),
                    },
                }),
                imports: vec![
                    ImportDeclaration {
                        loc: &b.location,
                        path: StringLiteral {
                            loc: &b.location,
                            type_info: type_info(fresher),
                            value: "path/foo".to_string(),
                        },
                        alias: None,
                    },
                    ImportDeclaration {
                        loc: &b.location,
                        path: StringLiteral {
                            loc: &b.location,
                            type_info: type_info(fresher),
                            value: "path/bar".to_string(),
                        },
                        alias: Some(Identifier {
                            loc: &b.location,
                            name: "b".to_string(),
                        }),
                    },
                ],
                body: Vec::new(),
            }],
        };
        let got = analyze_package(&pkg, fresher).unwrap();
        assert_eq!(want, got);
    }

    #[test]
    fn test_analyze_var_assignment() {
        let b = ast::BaseNode::default();
        let fresher = &mut TestFresher {};
        let pkg = ast::Package {
            base: b.clone(),
            path: "path".to_string(),
            package: "main".to_string(),
            files: vec![ast::File {
                base: b.clone(),
                name: "foo.flux".to_string(),
                package: None,
                imports: Vec::new(),
                body: vec![
                    ast::Statement::Var(ast::VariableAssignment {
                        base: b.clone(),
                        id: ast::Identifier {
                            base: b.clone(),
                            name: "a".to_string(),
                        },
                        init: ast::Expression::Bool(ast::BooleanLiteral {
                            base: b.clone(),
                            value: true,
                        }),
                    }),
                    ast::Statement::Expr(ast::ExpressionStatement {
                        base: b.clone(),
                        expression: ast::Expression::Idt(ast::Identifier {
                            base: b.clone(),
                            name: "a".to_string(),
                        }),
                    }),
                ],
            }],
        };
        let want = Package {
            loc: &b.location,
            package: "main".to_string(),
            files: vec![File {
                loc: &b.location,
                package: None,
                imports: Vec::new(),
                body: vec![
                    Statement::Var(VariableAssignment {
                        loc: &b.location,
                        id: Identifier {
                            loc: &b.location,
                            name: "a".to_string(),
                        },
                        init: Expression::Bool(BooleanLiteral {
                            loc: &b.location,
                            type_info: type_info(fresher),
                            value: true,
                        }),
                    }),
                    Statement::Expr(ExpressionStatement {
                        loc: &b.location,
                        expression: Expression::Idt(IdentifierExpression {
                            loc: &b.location,
                            type_info: type_info(fresher),
                            name: "a".to_string(),
                        }),
                    }),
                ],
            }],
        };
        let got = analyze_package(&pkg, fresher).unwrap();
        assert_eq!(want, got);
    }

    #[test]
    fn test_analyze_object() {
        let b = ast::BaseNode::default();
        let fresher = &mut TestFresher {};
        let pkg = ast::Package {
            base: b.clone(),
            path: "path".to_string(),
            package: "main".to_string(),
            files: vec![ast::File {
                base: b.clone(),
                name: "foo.flux".to_string(),
                package: None,
                imports: Vec::new(),
                body: vec![ast::Statement::Expr(ast::ExpressionStatement {
                    base: b.clone(),
                    expression: ast::Expression::Obj(Box::new(ast::ObjectExpression {
                        base: b.clone(),
                        with: None,
                        properties: vec![ast::Property {
                            base: b.clone(),
                            key: ast::PropertyKey::Identifier(ast::Identifier {
                                base: b.clone(),
                                name: "a".to_string(),
                            }),
                            value: Some(ast::Expression::Int(ast::IntegerLiteral {
                                base: b.clone(),
                                value: 10,
                            })),
                        }],
                    })),
                })],
            }],
        };
        let want = Package {
            loc: &b.location,
            package: "main".to_string(),
            files: vec![File {
                loc: &b.location,
                package: None,
                imports: Vec::new(),
                body: vec![Statement::Expr(ExpressionStatement {
                    loc: &b.location,
                    expression: Expression::Obj(Box::new(ObjectExpression {
                        loc: &b.location,
                        type_info: type_info(fresher),
                        with: None,
                        properties: vec![Property {
                            loc: &b.location,
                            key: PropertyKey::Identifier(Identifier {
                                loc: &b.location,
                                name: "a".to_string(),
                            }),
                            value: Some(Expression::Int(IntegerLiteral {
                                loc: &b.location,
                                type_info: type_info(fresher),
                                value: 10,
                            })),
                        }],
                    })),
                })],
            }],
        };
        let got = analyze_package(&pkg, fresher).unwrap();
        assert_eq!(want, got);
    }

    #[test]
    fn test_analyze_object_with_string_key() {
        let b = ast::BaseNode::default();
        let fresher = &mut TestFresher {};
        let pkg = ast::Package {
            base: b.clone(),
            path: "path".to_string(),
            package: "main".to_string(),
            files: vec![ast::File {
                base: b.clone(),
                name: "foo.flux".to_string(),
                package: None,
                imports: Vec::new(),
                body: vec![ast::Statement::Expr(ast::ExpressionStatement {
                    base: b.clone(),
                    expression: ast::Expression::Obj(Box::new(ast::ObjectExpression {
                        base: b.clone(),
                        with: None,
                        properties: vec![ast::Property {
                            base: b.clone(),
                            key: ast::PropertyKey::StringLiteral(ast::StringLiteral {
                                base: b.clone(),
                                value: "a".to_string(),
                            }),
                            value: Some(ast::Expression::Int(ast::IntegerLiteral {
                                base: b.clone(),
                                value: 10,
                            })),
                        }],
                    })),
                })],
            }],
        };
        let want = Package {
            loc: &b.location,
            package: "main".to_string(),
            files: vec![File {
                loc: &b.location,
                package: None,
                imports: Vec::new(),
                body: vec![Statement::Expr(ExpressionStatement {
                    loc: &b.location,
                    expression: Expression::Obj(Box::new(ObjectExpression {
                        loc: &b.location,
                        type_info: type_info(fresher),
                        with: None,
                        properties: vec![Property {
                            loc: &b.location,
                            key: PropertyKey::StringLiteral(StringLiteral {
                                loc: &b.location,
                                type_info: type_info(fresher),
                                value: "a".to_string(),
                            }),
                            value: Some(Expression::Int(IntegerLiteral {
                                loc: &b.location,
                                type_info: type_info(fresher),
                                value: 10,
                            })),
                        }],
                    })),
                })],
            }],
        };
        let got = analyze_package(&pkg, fresher).unwrap();
        assert_eq!(want, got);
    }

    #[test]
    fn test_analyze_object_with_mixed_keys() {
        let b = ast::BaseNode::default();
        let fresher = &mut TestFresher {};
        let pkg = ast::Package {
            base: b.clone(),
            path: "path".to_string(),
            package: "main".to_string(),
            files: vec![ast::File {
                base: b.clone(),
                name: "foo.flux".to_string(),
                package: None,
                imports: Vec::new(),
                body: vec![ast::Statement::Expr(ast::ExpressionStatement {
                    base: b.clone(),
                    expression: ast::Expression::Obj(Box::new(ast::ObjectExpression {
                        base: b.clone(),
                        with: None,
                        properties: vec![
                            ast::Property {
                                base: b.clone(),
                                key: ast::PropertyKey::StringLiteral(ast::StringLiteral {
                                    base: b.clone(),
                                    value: "a".to_string(),
                                }),
                                value: Some(ast::Expression::Int(ast::IntegerLiteral {
                                    base: b.clone(),
                                    value: 10,
                                })),
                            },
                            ast::Property {
                                base: b.clone(),
                                key: ast::PropertyKey::Identifier(ast::Identifier {
                                    base: b.clone(),
                                    name: "b".to_string(),
                                }),
                                value: Some(ast::Expression::Int(ast::IntegerLiteral {
                                    base: b.clone(),
                                    value: 11,
                                })),
                            },
                        ],
                    })),
                })],
            }],
        };
        let want = Package {
            loc: &b.location,
            package: "main".to_string(),
            files: vec![File {
                loc: &b.location,
                package: None,
                imports: Vec::new(),
                body: vec![Statement::Expr(ExpressionStatement {
                    loc: &b.location,
                    expression: Expression::Obj(Box::new(ObjectExpression {
                        loc: &b.location,
                        type_info: type_info(fresher),
                        with: None,
                        properties: vec![
                            Property {
                                loc: &b.location,
                                key: PropertyKey::StringLiteral(StringLiteral {
                                    loc: &b.location,
                                    type_info: type_info(fresher),
                                    value: "a".to_string(),
                                }),
                                value: Some(Expression::Int(IntegerLiteral {
                                    loc: &b.location,
                                    type_info: type_info(fresher),
                                    value: 10,
                                })),
                            },
                            Property {
                                loc: &b.location,
                                key: PropertyKey::Identifier(Identifier {
                                    loc: &b.location,
                                    name: "b".to_string(),
                                }),
                                value: Some(Expression::Int(IntegerLiteral {
                                    loc: &b.location,
                                    type_info: type_info(fresher),
                                    value: 11,
                                })),
                            },
                        ],
                    })),
                })],
            }],
        };
        let got = analyze_package(&pkg, fresher).unwrap();
        assert_eq!(want, got);
    }

    #[test]
    fn test_analyze_object_with_implicit_keys() {
        let b = ast::BaseNode::default();
        let fresher = &mut TestFresher {};
        let pkg = ast::Package {
            base: b.clone(),
            path: "path".to_string(),
            package: "main".to_string(),
            files: vec![ast::File {
                base: b.clone(),
                name: "foo.flux".to_string(),
                package: None,
                imports: Vec::new(),
                body: vec![ast::Statement::Expr(ast::ExpressionStatement {
                    base: b.clone(),
                    expression: ast::Expression::Obj(Box::new(ast::ObjectExpression {
                        base: b.clone(),
                        with: None,
                        properties: vec![
                            ast::Property {
                                base: b.clone(),
                                key: ast::PropertyKey::Identifier(ast::Identifier {
                                    base: b.clone(),
                                    name: "a".to_string(),
                                }),
                                value: None,
                            },
                            ast::Property {
                                base: b.clone(),
                                key: ast::PropertyKey::Identifier(ast::Identifier {
                                    base: b.clone(),
                                    name: "b".to_string(),
                                }),
                                value: None,
                            },
                        ],
                    })),
                })],
            }],
        };
        let want = Package {
            loc: &b.location,
            package: "main".to_string(),
            files: vec![File {
                loc: &b.location,
                package: None,
                imports: Vec::new(),
                body: vec![Statement::Expr(ExpressionStatement {
                    loc: &b.location,
                    expression: Expression::Obj(Box::new(ObjectExpression {
                        loc: &b.location,
                        type_info: type_info(fresher),
                        with: None,
                        properties: vec![
                            Property {
                                loc: &b.location,
                                key: PropertyKey::Identifier(Identifier {
                                    loc: &b.location,
                                    name: "a".to_string(),
                                }),
                                value: Some(Expression::Idt(IdentifierExpression {
                                    loc: &b.location,
                                    type_info: type_info(fresher),
                                    name: "a".to_string(),
                                })),
                            },
                            Property {
                                loc: &b.location,
                                key: PropertyKey::Identifier(Identifier {
                                    loc: &b.location,
                                    name: "b".to_string(),
                                }),
                                value: Some(Expression::Idt(IdentifierExpression {
                                    loc: &b.location,
                                    type_info: type_info(fresher),
                                    name: "b".to_string(),
                                })),
                            },
                        ],
                    })),
                })],
            }],
        };
        let got = analyze_package(&pkg, fresher).unwrap();
        assert_eq!(want, got);
    }

    #[test]
    fn test_analyze_options_declaration() {
        let b = ast::BaseNode::default();
        let fresher = &mut TestFresher {};
        let pkg = ast::Package {
            base: b.clone(),
            path: "path".to_string(),
            package: "main".to_string(),
            files: vec![ast::File {
                base: b.clone(),
                name: "foo.flux".to_string(),
                package: None,
                imports: Vec::new(),
                body: vec![ast::Statement::Opt(ast::OptionStatement {
                    base: b.clone(),
                    assignment: ast::Assignment::Variable(ast::VariableAssignment {
                        base: b.clone(),
                        id: ast::Identifier {
                            base: b.clone(),
                            name: "task".to_string(),
                        },
                        init: ast::Expression::Obj(Box::new(ast::ObjectExpression {
                            base: b.clone(),
                            with: None,
                            properties: vec![
                                ast::Property {
                                    base: b.clone(),
                                    key: ast::PropertyKey::Identifier(ast::Identifier {
                                        base: b.clone(),
                                        name: "name".to_string(),
                                    }),
                                    value: Some(ast::Expression::Str(ast::StringLiteral {
                                        base: b.clone(),
                                        value: "foo".to_string(),
                                    })),
                                },
                                ast::Property {
                                    base: b.clone(),
                                    key: ast::PropertyKey::Identifier(ast::Identifier {
                                        base: b.clone(),
                                        name: "every".to_string(),
                                    }),
                                    value: Some(ast::Expression::Dur(ast::DurationLiteral {
                                        base: b.clone(),
                                        values: vec![ast::Duration {
                                            magnitude: 1,
                                            unit: "h".to_string(),
                                        }],
                                    })),
                                },
                                ast::Property {
                                    base: b.clone(),
                                    key: ast::PropertyKey::Identifier(ast::Identifier {
                                        base: b.clone(),
                                        name: "delay".to_string(),
                                    }),
                                    value: Some(ast::Expression::Dur(ast::DurationLiteral {
                                        base: b.clone(),
                                        values: vec![ast::Duration {
                                            magnitude: 10,
                                            unit: "m".to_string(),
                                        }],
                                    })),
                                },
                                ast::Property {
                                    base: b.clone(),
                                    key: ast::PropertyKey::Identifier(ast::Identifier {
                                        base: b.clone(),
                                        name: "cron".to_string(),
                                    }),
                                    value: Some(ast::Expression::Str(ast::StringLiteral {
                                        base: b.clone(),
                                        value: "0 2 * * *".to_string(),
                                    })),
                                },
                                ast::Property {
                                    base: b.clone(),
                                    key: ast::PropertyKey::Identifier(ast::Identifier {
                                        base: b.clone(),
                                        name: "retry".to_string(),
                                    }),
                                    value: Some(ast::Expression::Int(ast::IntegerLiteral {
                                        base: b.clone(),
                                        value: 5,
                                    })),
                                },
                            ],
                        })),
                    }),
                })],
            }],
        };
        let want = Package {
            loc: &b.location,
            package: "main".to_string(),
            files: vec![File {
                loc: &b.location,
                package: None,
                imports: Vec::new(),
                body: vec![Statement::Opt(OptionStatement {
                    loc: &b.location,
                    assignment: Assignment::Variable(VariableAssignment {
                        loc: &b.location,
                        id: Identifier {
                            loc: &b.location,
                            name: "task".to_string(),
                        },
                        init: Expression::Obj(Box::new(ObjectExpression {
                            loc: &b.location,
                            type_info: type_info(fresher),
                            with: None,
                            properties: vec![
                                Property {
                                    loc: &b.location,
                                    key: PropertyKey::Identifier(Identifier {
                                        loc: &b.location,
                                        name: "name".to_string(),
                                    }),
                                    value: Some(Expression::Str(StringLiteral {
                                        loc: &b.location,
                                        type_info: type_info(fresher),
                                        value: "foo".to_string(),
                                    })),
                                },
                                Property {
                                    loc: &b.location,
                                    key: PropertyKey::Identifier(Identifier {
                                        loc: &b.location,
                                        name: "every".to_string(),
                                    }),
                                    value: Some(Expression::Dur(DurationLiteral {
                                        loc: &b.location,
                                        type_info: type_info(fresher),
                                        value: chrono::Duration::hours(1),
                                    })),
                                },
                                Property {
                                    loc: &b.location,
                                    key: PropertyKey::Identifier(Identifier {
                                        loc: &b.location,
                                        name: "delay".to_string(),
                                    }),
                                    value: Some(Expression::Dur(DurationLiteral {
                                        loc: &b.location,
                                        type_info: type_info(fresher),
                                        value: chrono::Duration::minutes(10),
                                    })),
                                },
                                Property {
                                    loc: &b.location,
                                    key: PropertyKey::Identifier(Identifier {
                                        loc: &b.location,
                                        name: "cron".to_string(),
                                    }),
                                    value: Some(Expression::Str(StringLiteral {
                                        loc: &b.location,
                                        type_info: type_info(fresher),
                                        value: "0 2 * * *".to_string(),
                                    })),
                                },
                                Property {
                                    loc: &b.location,
                                    key: PropertyKey::Identifier(Identifier {
                                        loc: &b.location,
                                        name: "retry".to_string(),
                                    }),
                                    value: Some(Expression::Int(IntegerLiteral {
                                        loc: &b.location,
                                        type_info: type_info(fresher),
                                        value: 5,
                                    })),
                                },
                            ],
                        })),
                    }),
                })],
            }],
        };
        let got = analyze_package(&pkg, fresher).unwrap();
        assert_eq!(want, got);
    }

    #[test]
    fn test_analyze_qualified_option_statement() {
        let b = ast::BaseNode::default();
        let fresher = &mut TestFresher {};
        let pkg = ast::Package {
            base: b.clone(),
            path: "path".to_string(),
            package: "main".to_string(),
            files: vec![ast::File {
                base: b.clone(),
                name: "foo.flux".to_string(),
                package: None,
                imports: Vec::new(),
                body: vec![ast::Statement::Opt(ast::OptionStatement {
                    base: b.clone(),
                    assignment: ast::Assignment::Member(ast::MemberAssignment {
                        base: b.clone(),
                        member: ast::MemberExpression {
                            base: b.clone(),
                            object: ast::Expression::Idt(ast::Identifier {
                                base: b.clone(),
                                name: "alert".to_string(),
                            }),
                            property: ast::PropertyKey::Identifier(ast::Identifier {
                                base: b.clone(),
                                name: "state".to_string(),
                            }),
                        },
                        init: ast::Expression::Str(ast::StringLiteral {
                            base: b.clone(),
                            value: "Warning".to_string(),
                        }),
                    }),
                })],
            }],
        };
        let want = Package {
            loc: &b.location,
            package: "main".to_string(),
            files: vec![File {
                loc: &b.location,
                package: None,
                imports: Vec::new(),
                body: vec![Statement::Opt(OptionStatement {
                    loc: &b.location,
                    assignment: Assignment::Member(MemberAssignment {
                        loc: &b.location,
                        member: MemberExpression {
                            loc: &b.location,
                            type_info: type_info(fresher),
                            object: Expression::Idt(IdentifierExpression {
                                loc: &b.location,
                                type_info: type_info(fresher),
                                name: "alert".to_string(),
                            }),
                            property: "state".to_string(),
                        },
                        init: Expression::Str(StringLiteral {
                            loc: &b.location,
                            type_info: type_info(fresher),
                            value: "Warning".to_string(),
                        }),
                    }),
                })],
            }],
        };
        let got = analyze_package(&pkg, fresher).unwrap();
        assert_eq!(want, got);
    }

    #[test]
    fn test_analyze_function() {
        let b = ast::BaseNode::default();
        let fresher = &mut TestFresher {};
        let pkg = ast::Package {
            base: b.clone(),
            path: "path".to_string(),
            package: "main".to_string(),
            files: vec![ast::File {
                base: b.clone(),
                name: "foo.flux".to_string(),
                package: None,
                imports: Vec::new(),
                body: vec![
                    ast::Statement::Var(ast::VariableAssignment {
                        base: b.clone(),
                        id: ast::Identifier {
                            base: b.clone(),
                            name: "f".to_string(),
                        },
                        init: ast::Expression::Fun(Box::new(ast::FunctionExpression {
                            base: b.clone(),
                            params: vec![
                                ast::Property {
                                    base: b.clone(),
                                    key: ast::PropertyKey::Identifier(ast::Identifier {
                                        base: b.clone(),
                                        name: "a".to_string(),
                                    }),
                                    value: None,
                                },
                                ast::Property {
                                    base: b.clone(),
                                    key: ast::PropertyKey::Identifier(ast::Identifier {
                                        base: b.clone(),
                                        name: "b".to_string(),
                                    }),
                                    value: None,
                                },
                            ],
                            body: ast::FunctionBody::Expr(ast::Expression::Bin(Box::new(
                                ast::BinaryExpression {
                                    base: b.clone(),
                                    operator: ast::OperatorKind::AdditionOperator,
                                    left: ast::Expression::Idt(ast::Identifier {
                                        base: b.clone(),
                                        name: "a".to_string(),
                                    }),
                                    right: ast::Expression::Idt(ast::Identifier {
                                        base: b.clone(),
                                        name: "b".to_string(),
                                    }),
                                },
                            ))),
                        })),
                    }),
                    ast::Statement::Expr(ast::ExpressionStatement {
                        base: b.clone(),
                        expression: ast::Expression::Call(Box::new(ast::CallExpression {
                            base: b.clone(),
                            callee: ast::Expression::Idt(ast::Identifier {
                                base: b.clone(),
                                name: "f".to_string(),
                            }),
                            arguments: vec![ast::Expression::Obj(Box::new(
                                ast::ObjectExpression {
                                    base: b.clone(),
                                    with: None,
                                    properties: vec![
                                        ast::Property {
                                            base: b.clone(),
                                            key: ast::PropertyKey::Identifier(ast::Identifier {
                                                base: b.clone(),
                                                name: "a".to_string(),
                                            }),
                                            value: Some(ast::Expression::Int(
                                                ast::IntegerLiteral {
                                                    base: b.clone(),
                                                    value: 2,
                                                },
                                            )),
                                        },
                                        ast::Property {
                                            base: b.clone(),
                                            key: ast::PropertyKey::Identifier(ast::Identifier {
                                                base: b.clone(),
                                                name: "b".to_string(),
                                            }),
                                            value: Some(ast::Expression::Int(
                                                ast::IntegerLiteral {
                                                    base: b.clone(),
                                                    value: 3,
                                                },
                                            )),
                                        },
                                    ],
                                },
                            ))],
                        })),
                    }),
                ],
            }],
        };
        let want = Package {
            loc: &b.location,
            package: "main".to_string(),
            files: vec![File {
                loc: &b.location,
                package: None,
                imports: Vec::new(),
                body: vec![
                    Statement::Var(VariableAssignment {
                        loc: &b.location,
                        id: Identifier {
                            loc: &b.location,
                            name: "f".to_string(),
                        },
                        init: Expression::Fun(Box::new(FunctionExpression {
                            loc: &b.location,
                            type_info: type_info(fresher),
                            params: vec![
                                FunctionParameter {
                                    loc: &b.location,
                                    is_pipe: false,
                                    key: Identifier {
                                        loc: &b.location,
                                        name: "a".to_string(),
                                    },
                                    default: None,
                                },
                                FunctionParameter {
                                    loc: &b.location,
                                    is_pipe: false,
                                    key: Identifier {
                                        loc: &b.location,
                                        name: "b".to_string(),
                                    },
                                    default: None,
                                },
                            ],
                            body: FunctionBody::Expr(Expression::Bin(Box::new(BinaryExpression {
                                loc: &b.location,
                                type_info: type_info(fresher),
                                operator: ast::OperatorKind::AdditionOperator,
                                left: Expression::Idt(IdentifierExpression {
                                    loc: &b.location,
                                    type_info: type_info(fresher),
                                    name: "a".to_string(),
                                }),
                                right: Expression::Idt(IdentifierExpression {
                                    loc: &b.location,
                                    type_info: type_info(fresher),
                                    name: "b".to_string(),
                                }),
                            }))),
                        })),
                    }),
                    Statement::Expr(ExpressionStatement {
                        loc: &b.location,
                        expression: Expression::Call(Box::new(CallExpression {
                            loc: &b.location,
                            type_info: type_info(fresher),
                            pipe: None,
                            callee: Expression::Idt(IdentifierExpression {
                                loc: &b.location,
                                type_info: type_info(fresher),
                                name: "f".to_string(),
                            }),
                            arguments: ObjectExpression {
                                loc: &b.location,
                                type_info: type_info(fresher),
                                with: None,
                                properties: vec![
                                    Property {
                                        loc: &b.location,
                                        key: PropertyKey::Identifier(Identifier {
                                            loc: &b.location,
                                            name: "a".to_string(),
                                        }),
                                        value: Some(Expression::Int(IntegerLiteral {
                                            loc: &b.location,
                                            type_info: type_info(fresher),
                                            value: 2,
                                        })),
                                    },
                                    Property {
                                        loc: &b.location,
                                        key: PropertyKey::Identifier(Identifier {
                                            loc: &b.location,
                                            name: "b".to_string(),
                                        }),
                                        value: Some(Expression::Int(IntegerLiteral {
                                            loc: &b.location,
                                            type_info: type_info(fresher),
                                            value: 3,
                                        })),
                                    },
                                ],
                            },
                        })),
                    }),
                ],
            }],
        };
        let got = analyze_package(&pkg, fresher).unwrap();
        assert_eq!(want, got);
    }

    #[test]
    fn test_analyze_function_with_defaults() {
        let b = ast::BaseNode::default();
        let fresher = &mut TestFresher {};
        let pkg = ast::Package {
            base: b.clone(),
            path: "path".to_string(),
            package: "main".to_string(),
            files: vec![ast::File {
                base: b.clone(),
                name: "foo.flux".to_string(),
                package: None,
                imports: Vec::new(),
                body: vec![
                    ast::Statement::Var(ast::VariableAssignment {
                        base: b.clone(),
                        id: ast::Identifier {
                            base: b.clone(),
                            name: "f".to_string(),
                        },
                        init: ast::Expression::Fun(Box::new(ast::FunctionExpression {
                            base: b.clone(),
                            params: vec![
                                ast::Property {
                                    base: b.clone(),
                                    key: ast::PropertyKey::Identifier(ast::Identifier {
                                        base: b.clone(),
                                        name: "a".to_string(),
                                    }),
                                    value: Some(ast::Expression::Int(ast::IntegerLiteral {
                                        base: b.clone(),
                                        value: 0,
                                    })),
                                },
                                ast::Property {
                                    base: b.clone(),
                                    key: ast::PropertyKey::Identifier(ast::Identifier {
                                        base: b.clone(),
                                        name: "b".to_string(),
                                    }),
                                    value: Some(ast::Expression::Int(ast::IntegerLiteral {
                                        base: b.clone(),
                                        value: 0,
                                    })),
                                },
                                ast::Property {
                                    base: b.clone(),
                                    key: ast::PropertyKey::Identifier(ast::Identifier {
                                        base: b.clone(),
                                        name: "c".to_string(),
                                    }),
                                    value: None,
                                },
                            ],
                            body: ast::FunctionBody::Expr(ast::Expression::Bin(Box::new(
                                ast::BinaryExpression {
                                    base: b.clone(),
                                    operator: ast::OperatorKind::AdditionOperator,
                                    left: ast::Expression::Bin(Box::new(ast::BinaryExpression {
                                        base: b.clone(),
                                        operator: ast::OperatorKind::AdditionOperator,
                                        left: ast::Expression::Idt(ast::Identifier {
                                            base: b.clone(),
                                            name: "a".to_string(),
                                        }),
                                        right: ast::Expression::Idt(ast::Identifier {
                                            base: b.clone(),
                                            name: "b".to_string(),
                                        }),
                                    })),
                                    right: ast::Expression::Idt(ast::Identifier {
                                        base: b.clone(),
                                        name: "c".to_string(),
                                    }),
                                },
                            ))),
                        })),
                    }),
                    ast::Statement::Expr(ast::ExpressionStatement {
                        base: b.clone(),
                        expression: ast::Expression::Call(Box::new(ast::CallExpression {
                            base: b.clone(),
                            callee: ast::Expression::Idt(ast::Identifier {
                                base: b.clone(),
                                name: "f".to_string(),
                            }),
                            arguments: vec![ast::Expression::Obj(Box::new(
                                ast::ObjectExpression {
                                    base: b.clone(),
                                    with: None,
                                    properties: vec![ast::Property {
                                        base: b.clone(),
                                        key: ast::PropertyKey::Identifier(ast::Identifier {
                                            base: b.clone(),
                                            name: "c".to_string(),
                                        }),
                                        value: Some(ast::Expression::Int(ast::IntegerLiteral {
                                            base: b.clone(),
                                            value: 42,
                                        })),
                                    }],
                                },
                            ))],
                        })),
                    }),
                ],
            }],
        };
        let want = Package {
            loc: &b.location,
            package: "main".to_string(),
            files: vec![File {
                loc: &b.location,
                package: None,
                imports: Vec::new(),
                body: vec![
                    Statement::Var(VariableAssignment {
                        loc: &b.location,
                        id: Identifier {
                            loc: &b.location,
                            name: "f".to_string(),
                        },
                        init: Expression::Fun(Box::new(FunctionExpression {
                            loc: &b.location,
                            type_info: type_info(fresher),
                            params: vec![
                                FunctionParameter {
                                    loc: &b.location,
                                    is_pipe: false,
                                    key: Identifier {
                                        loc: &b.location,
                                        name: "a".to_string(),
                                    },
                                    default: Some(Expression::Int(IntegerLiteral {
                                        loc: &b.location,
                                        type_info: type_info(fresher),
                                        value: 0,
                                    })),
                                },
                                FunctionParameter {
                                    loc: &b.location,
                                    is_pipe: false,
                                    key: Identifier {
                                        loc: &b.location,
                                        name: "b".to_string(),
                                    },
                                    default: Some(Expression::Int(IntegerLiteral {
                                        loc: &b.location,
                                        type_info: type_info(fresher),
                                        value: 0,
                                    })),
                                },
                                FunctionParameter {
                                    loc: &b.location,
                                    is_pipe: false,
                                    key: Identifier {
                                        loc: &b.location,
                                        name: "c".to_string(),
                                    },
                                    default: None,
                                },
                            ],
                            body: FunctionBody::Expr(Expression::Bin(Box::new(BinaryExpression {
                                loc: &b.location,
                                type_info: type_info(fresher),
                                operator: ast::OperatorKind::AdditionOperator,
                                left: Expression::Bin(Box::new(BinaryExpression {
                                    loc: &b.location,
                                    type_info: type_info(fresher),
                                    operator: ast::OperatorKind::AdditionOperator,
                                    left: Expression::Idt(IdentifierExpression {
                                        loc: &b.location,
                                        type_info: type_info(fresher),
                                        name: "a".to_string(),
                                    }),
                                    right: Expression::Idt(IdentifierExpression {
                                        loc: &b.location,
                                        type_info: type_info(fresher),
                                        name: "b".to_string(),
                                    }),
                                })),
                                right: Expression::Idt(IdentifierExpression {
                                    loc: &b.location,
                                    type_info: type_info(fresher),
                                    name: "c".to_string(),
                                }),
                            }))),
                        })),
                    }),
                    Statement::Expr(ExpressionStatement {
                        loc: &b.location,
                        expression: Expression::Call(Box::new(CallExpression {
                            loc: &b.location,
                            type_info: type_info(fresher),
                            pipe: None,
                            callee: Expression::Idt(IdentifierExpression {
                                loc: &b.location,
                                type_info: type_info(fresher),
                                name: "f".to_string(),
                            }),
                            arguments: ObjectExpression {
                                loc: &b.location,
                                type_info: type_info(fresher),
                                with: None,
                                properties: vec![Property {
                                    loc: &b.location,
                                    key: PropertyKey::Identifier(Identifier {
                                        loc: &b.location,
                                        name: "c".to_string(),
                                    }),
                                    value: Some(Expression::Int(IntegerLiteral {
                                        loc: &b.location,
                                        type_info: type_info(fresher),
                                        value: 42,
                                    })),
                                }],
                            },
                        })),
                    }),
                ],
            }],
        };
        let got = analyze_package(&pkg, fresher).unwrap();
        assert_eq!(want, got);
    }

    #[test]
    fn test_analyze_function_multiple_pipes() {
        let b = ast::BaseNode::default();
        let fresher = &mut TestFresher {};
        let pkg = ast::Package {
            base: b.clone(),
            path: "path".to_string(),
            package: "main".to_string(),
            files: vec![ast::File {
                base: b.clone(),
                name: "foo.flux".to_string(),
                package: None,
                imports: Vec::new(),
                body: vec![ast::Statement::Var(ast::VariableAssignment {
                    base: b.clone(),
                    id: ast::Identifier {
                        base: b.clone(),
                        name: "f".to_string(),
                    },
                    init: ast::Expression::Fun(Box::new(ast::FunctionExpression {
                        base: b.clone(),
                        params: vec![
                            ast::Property {
                                base: b.clone(),
                                key: ast::PropertyKey::Identifier(ast::Identifier {
                                    base: b.clone(),
                                    name: "a".to_string(),
                                }),
                                value: None,
                            },
                            ast::Property {
                                base: b.clone(),
                                key: ast::PropertyKey::Identifier(ast::Identifier {
                                    base: b.clone(),
                                    name: "piped1".to_string(),
                                }),
                                value: Some(ast::Expression::PipeLit(ast::PipeLiteral {
                                    base: b.clone(),
                                })),
                            },
                            ast::Property {
                                base: b.clone(),
                                key: ast::PropertyKey::Identifier(ast::Identifier {
                                    base: b.clone(),
                                    name: "piped2".to_string(),
                                }),
                                value: Some(ast::Expression::PipeLit(ast::PipeLiteral {
                                    base: b.clone(),
                                })),
                            },
                        ],
                        body: ast::FunctionBody::Expr(ast::Expression::Idt(ast::Identifier {
                            base: b.clone(),
                            name: "a".to_string(),
                        })),
                    })),
                })],
            }],
        };
        let got = analyze_package(&pkg, fresher).err().unwrap().to_string();
        assert_eq!("only a single argument may be piped".to_string(), got);
    }

    #[test]
    fn test_analyze_call_multiple_object_arguments() {
        let b = ast::BaseNode::default();
        let fresher = &mut TestFresher {};
        let pkg = ast::Package {
            base: b.clone(),
            path: "path".to_string(),
            package: "main".to_string(),
            files: vec![ast::File {
                base: b.clone(),
                name: "foo.flux".to_string(),
                package: None,
                imports: Vec::new(),
                body: vec![ast::Statement::Expr(ast::ExpressionStatement {
                    base: b.clone(),
                    expression: ast::Expression::Call(Box::new(ast::CallExpression {
                        base: b.clone(),
                        callee: ast::Expression::Idt(ast::Identifier {
                            base: b.clone(),
                            name: "f".to_string(),
                        }),
                        arguments: vec![
                            ast::Expression::Obj(Box::new(ast::ObjectExpression {
                                base: b.clone(),
                                with: None,
                                properties: vec![ast::Property {
                                    base: b.clone(),
                                    key: ast::PropertyKey::Identifier(ast::Identifier {
                                        base: b.clone(),
                                        name: "a".to_string(),
                                    }),
                                    value: Some(ast::Expression::Int(ast::IntegerLiteral {
                                        base: b.clone(),
                                        value: 0,
                                    })),
                                }],
                            })),
                            ast::Expression::Obj(Box::new(ast::ObjectExpression {
                                base: b.clone(),
                                with: None,
                                properties: vec![ast::Property {
                                    base: b.clone(),
                                    key: ast::PropertyKey::Identifier(ast::Identifier {
                                        base: b.clone(),
                                        name: "b".to_string(),
                                    }),
                                    value: Some(ast::Expression::Int(ast::IntegerLiteral {
                                        base: b.clone(),
                                        value: 1,
                                    })),
                                }],
                            })),
                        ],
                    })),
                })],
            }],
        };
        let got = analyze_package(&pkg, fresher).err().unwrap().to_string();
        assert_eq!(
            "arguments are not a single object expression".to_string(),
            got
        );
    }

    #[test]
    fn test_analyze_pipe_expression() {
        let b = ast::BaseNode::default();
        let fresher = &mut TestFresher {};
        let pkg = ast::Package {
            base: b.clone(),
            path: "path".to_string(),
            package: "main".to_string(),
            files: vec![ast::File {
                base: b.clone(),
                name: "foo.flux".to_string(),
                package: None,
                imports: Vec::new(),
                body: vec![
                    ast::Statement::Var(ast::VariableAssignment {
                        base: b.clone(),
                        id: ast::Identifier {
                            base: b.clone(),
                            name: "f".to_string(),
                        },
                        init: ast::Expression::Fun(Box::new(ast::FunctionExpression {
                            base: b.clone(),
                            params: vec![
                                ast::Property {
                                    base: b.clone(),
                                    key: ast::PropertyKey::Identifier(ast::Identifier {
                                        base: b.clone(),
                                        name: "piped".to_string(),
                                    }),
                                    value: Some(ast::Expression::PipeLit(ast::PipeLiteral {
                                        base: b.clone(),
                                    })),
                                },
                                ast::Property {
                                    base: b.clone(),
                                    key: ast::PropertyKey::Identifier(ast::Identifier {
                                        base: b.clone(),
                                        name: "a".to_string(),
                                    }),
                                    value: None,
                                },
                            ],
                            body: ast::FunctionBody::Expr(ast::Expression::Bin(Box::new(
                                ast::BinaryExpression {
                                    base: b.clone(),
                                    operator: ast::OperatorKind::AdditionOperator,
                                    left: ast::Expression::Idt(ast::Identifier {
                                        base: b.clone(),
                                        name: "a".to_string(),
                                    }),
                                    right: ast::Expression::Idt(ast::Identifier {
                                        base: b.clone(),
                                        name: "piped".to_string(),
                                    }),
                                },
                            ))),
                        })),
                    }),
                    ast::Statement::Expr(ast::ExpressionStatement {
                        base: b.clone(),
                        expression: ast::Expression::Pipe(Box::new(ast::PipeExpression {
                            base: b.clone(),
                            argument: ast::Expression::Int(ast::IntegerLiteral {
                                base: b.clone(),
                                value: 3,
                            }),
                            call: ast::CallExpression {
                                base: b.clone(),
                                callee: ast::Expression::Idt(ast::Identifier {
                                    base: b.clone(),
                                    name: "f".to_string(),
                                }),
                                arguments: vec![ast::Expression::Obj(Box::new(
                                    ast::ObjectExpression {
                                        base: b.clone(),
                                        with: None,
                                        properties: vec![ast::Property {
                                            base: b.clone(),
                                            key: ast::PropertyKey::Identifier(ast::Identifier {
                                                base: b.clone(),
                                                name: "a".to_string(),
                                            }),
                                            value: Some(ast::Expression::Int(
                                                ast::IntegerLiteral {
                                                    base: b.clone(),
                                                    value: 2,
                                                },
                                            )),
                                        }],
                                    },
                                ))],
                            },
                        })),
                    }),
                ],
            }],
        };
        let want = Package {
            loc: &b.location,
            package: "main".to_string(),
            files: vec![File {
                loc: &b.location,
                package: None,
                imports: Vec::new(),
                body: vec![
                    Statement::Var(VariableAssignment {
                        loc: &b.location,
                        id: Identifier {
                            loc: &b.location,
                            name: "f".to_string(),
                        },
                        init: Expression::Fun(Box::new(FunctionExpression {
                            loc: &b.location,
                            type_info: type_info(fresher),
                            params: vec![
                                FunctionParameter {
                                    loc: &b.location,
                                    is_pipe: true,
                                    key: Identifier {
                                        loc: &b.location,
                                        name: "piped".to_string(),
                                    },
                                    default: None,
                                },
                                FunctionParameter {
                                    loc: &b.location,
                                    is_pipe: false,
                                    key: Identifier {
                                        loc: &b.location,
                                        name: "a".to_string(),
                                    },
                                    default: None,
                                },
                            ],
                            body: FunctionBody::Expr(Expression::Bin(Box::new(BinaryExpression {
                                loc: &b.location,
                                type_info: type_info(fresher),
                                operator: ast::OperatorKind::AdditionOperator,
                                left: Expression::Idt(IdentifierExpression {
                                    loc: &b.location,
                                    type_info: type_info(fresher),
                                    name: "a".to_string(),
                                }),
                                right: Expression::Idt(IdentifierExpression {
                                    loc: &b.location,
                                    type_info: type_info(fresher),
                                    name: "piped".to_string(),
                                }),
                            }))),
                        })),
                    }),
                    Statement::Expr(ExpressionStatement {
                        loc: &b.location,
                        expression: Expression::Call(Box::new(CallExpression {
                            loc: &b.location,
                            type_info: type_info(fresher),
                            pipe: Some(Expression::Int(IntegerLiteral {
                                loc: &b.location,
                                type_info: type_info(fresher),
                                value: 3,
                            })),
                            callee: Expression::Idt(IdentifierExpression {
                                loc: &b.location,
                                type_info: type_info(fresher),
                                name: "f".to_string(),
                            }),
                            arguments: ObjectExpression {
                                loc: &b.location,
                                type_info: type_info(fresher),
                                with: None,
                                properties: vec![Property {
                                    loc: &b.location,
                                    key: PropertyKey::Identifier(Identifier {
                                        loc: &b.location,
                                        name: "a".to_string(),
                                    }),
                                    value: Some(Expression::Int(IntegerLiteral {
                                        loc: &b.location,
                                        type_info: type_info(fresher),
                                        value: 2,
                                    })),
                                }],
                            },
                        })),
                    }),
                ],
            }],
        };
        let got = analyze_package(&pkg, fresher).unwrap();
        assert_eq!(want, got);
    }

    #[test]
    fn test_function_expression_simple() {
        let b = ast::BaseNode::default();
        let fresher = &mut TestFresher {};
        let f = FunctionExpression {
            loc: &b.location,
            type_info: type_info(fresher),
            params: vec![
                FunctionParameter {
                    loc: &b.location,
                    is_pipe: false,
                    key: Identifier {
                        loc: &b.location,
                        name: "a".to_string(),
                    },
                    default: None,
                },
                FunctionParameter {
                    loc: &b.location,
                    is_pipe: false,
                    key: Identifier {
                        loc: &b.location,
                        name: "b".to_string(),
                    },
                    default: None,
                },
            ],
            body: FunctionBody::Expr(Expression::Bin(Box::new(BinaryExpression {
                loc: &b.location,
                type_info: type_info(fresher),
                operator: ast::OperatorKind::AdditionOperator,
                left: Expression::Idt(IdentifierExpression {
                    loc: &b.location,
                    type_info: type_info(fresher),
                    name: "a".to_string(),
                }),
                right: Expression::Idt(IdentifierExpression {
                    loc: &b.location,
                    type_info: type_info(fresher),
                    name: "b".to_string(),
                }),
            }))),
        };
        assert_eq!(Vec::<&FunctionParameter>::new(), f.defaults());
        assert_eq!(None, f.pipe());
    }

    #[test]
    fn test_function_expression_defaults_and_pipes() {
        let b = ast::BaseNode::default();
        let fresher = &mut TestFresher {};
        let piped = FunctionParameter {
            loc: &b.location,
            is_pipe: true,
            key: Identifier {
                loc: &b.location,
                name: "a".to_string(),
            },
            default: Some(Expression::Int(IntegerLiteral {
                loc: &b.location,
                type_info: type_info(fresher),
                value: 0,
            })),
        };
        let default1 = FunctionParameter {
            loc: &b.location,
            is_pipe: false,
            key: Identifier {
                loc: &b.location,
                name: "b".to_string(),
            },
            default: Some(Expression::Int(IntegerLiteral {
                loc: &b.location,
                type_info: type_info(fresher),
                value: 1,
            })),
        };
        let default2 = FunctionParameter {
            loc: &b.location,
            is_pipe: false,
            key: Identifier {
                loc: &b.location,
                name: "c".to_string(),
            },
            default: Some(Expression::Int(IntegerLiteral {
                loc: &b.location,
                type_info: type_info(fresher),
                value: 2,
            })),
        };
        let no_default = FunctionParameter {
            loc: &b.location,
            is_pipe: false,
            key: Identifier {
                loc: &b.location,
                name: "d".to_string(),
            },
            default: None,
        };
        let defaults = vec![&piped, &default1, &default2];
        let f = FunctionExpression {
            loc: &b.location,
            type_info: type_info(fresher),
            params: vec![
                piped.clone(),
                default1.clone(),
                default2.clone(),
                no_default.clone(),
            ],
            body: FunctionBody::Expr(Expression::Bin(Box::new(BinaryExpression {
                loc: &b.location,
                type_info: type_info(fresher),
                operator: ast::OperatorKind::AdditionOperator,
                left: Expression::Idt(IdentifierExpression {
                    loc: &b.location,
                    type_info: type_info(fresher),
                    name: "a".to_string(),
                }),
                right: Expression::Idt(IdentifierExpression {
                    loc: &b.location,
                    type_info: type_info(fresher),
                    name: "b".to_string(),
                }),
            }))),
        };
        assert_eq!(defaults, f.defaults());
        assert_eq!(Some(&piped), f.pipe());
    }

    #[test]
    fn test_analyze_index_expression() {
        let b = ast::BaseNode::default();
        let fresher = &mut TestFresher {};
        let pkg = ast::Package {
            base: b.clone(),
            path: "path".to_string(),
            package: "main".to_string(),
            files: vec![ast::File {
                base: b.clone(),
                name: "foo.flux".to_string(),
                package: None,
                imports: Vec::new(),
                body: vec![ast::Statement::Expr(ast::ExpressionStatement {
                    base: b.clone(),
                    expression: ast::Expression::Idx(Box::new(ast::IndexExpression {
                        base: b.clone(),
                        array: ast::Expression::Idt(ast::Identifier {
                            base: b.clone(),
                            name: "a".to_string(),
                        }),
                        index: ast::Expression::Int(ast::IntegerLiteral {
                            base: b.clone(),
                            value: 3,
                        }),
                    })),
                })],
            }],
        };
        let want = Package {
            loc: &b.location,
            package: "main".to_string(),
            files: vec![File {
                loc: &b.location,
                package: None,
                imports: Vec::new(),
                body: vec![Statement::Expr(ExpressionStatement {
                    loc: &b.location,
                    expression: Expression::Idx(Box::new(IndexExpression {
                        loc: &b.location,
                        type_info: type_info(fresher),
                        array: Expression::Idt(IdentifierExpression {
                            loc: &b.location,
                            type_info: type_info(fresher),
                            name: "a".to_string(),
                        }),
                        index: Expression::Int(IntegerLiteral {
                            loc: &b.location,
                            type_info: type_info(fresher),
                            value: 3,
                        }),
                    })),
                })],
            }],
        };
        let got = analyze_package(&pkg, fresher).unwrap();
        assert_eq!(want, got);
    }

    #[test]
    fn test_analyze_nested_index_expression() {
        let b = ast::BaseNode::default();
        let fresher = &mut TestFresher {};
        let pkg = ast::Package {
            base: b.clone(),
            path: "path".to_string(),
            package: "main".to_string(),
            files: vec![ast::File {
                base: b.clone(),
                name: "foo.flux".to_string(),
                package: None,
                imports: Vec::new(),
                body: vec![ast::Statement::Expr(ast::ExpressionStatement {
                    base: b.clone(),
                    expression: ast::Expression::Idx(Box::new(ast::IndexExpression {
                        base: b.clone(),
                        array: ast::Expression::Idx(Box::new(ast::IndexExpression {
                            base: b.clone(),
                            array: ast::Expression::Idt(ast::Identifier {
                                base: b.clone(),
                                name: "a".to_string(),
                            }),
                            index: ast::Expression::Int(ast::IntegerLiteral {
                                base: b.clone(),
                                value: 3,
                            }),
                        })),
                        index: ast::Expression::Int(ast::IntegerLiteral {
                            base: b.clone(),
                            value: 5,
                        }),
                    })),
                })],
            }],
        };
        let want = Package {
            loc: &b.location,
            package: "main".to_string(),
            files: vec![File {
                loc: &b.location,
                package: None,
                imports: Vec::new(),
                body: vec![Statement::Expr(ExpressionStatement {
                    loc: &b.location,
                    expression: Expression::Idx(Box::new(IndexExpression {
                        loc: &b.location,
                        type_info: type_info(fresher),
                        array: Expression::Idx(Box::new(IndexExpression {
                            loc: &b.location,
                            type_info: type_info(fresher),
                            array: Expression::Idt(IdentifierExpression {
                                loc: &b.location,
                                type_info: type_info(fresher),
                                name: "a".to_string(),
                            }),
                            index: Expression::Int(IntegerLiteral {
                                loc: &b.location,
                                type_info: type_info(fresher),
                                value: 3,
                            }),
                        })),
                        index: Expression::Int(IntegerLiteral {
                            loc: &b.location,
                            type_info: type_info(fresher),
                            value: 5,
                        }),
                    })),
                })],
            }],
        };
        let got = analyze_package(&pkg, fresher).unwrap();
        assert_eq!(want, got);
    }

    #[test]
    fn test_analyze_access_idexed_object_returned_from_function_call() {
        let b = ast::BaseNode::default();
        let fresher = &mut TestFresher {};
        let pkg = ast::Package {
            base: b.clone(),
            path: "path".to_string(),
            package: "main".to_string(),
            files: vec![ast::File {
                base: b.clone(),
                name: "foo.flux".to_string(),
                package: None,
                imports: Vec::new(),
                body: vec![ast::Statement::Expr(ast::ExpressionStatement {
                    base: b.clone(),
                    expression: ast::Expression::Idx(Box::new(ast::IndexExpression {
                        base: b.clone(),
                        array: ast::Expression::Call(Box::new(ast::CallExpression {
                            base: b.clone(),
                            callee: ast::Expression::Idt(ast::Identifier {
                                base: b.clone(),
                                name: "f".to_string(),
                            }),
                            arguments: vec![],
                        })),
                        index: ast::Expression::Int(ast::IntegerLiteral {
                            base: b.clone(),
                            value: 3,
                        }),
                    })),
                })],
            }],
        };
        let want = Package {
            loc: &b.location,
            package: "main".to_string(),
            files: vec![File {
                loc: &b.location,
                package: None,
                imports: Vec::new(),
                body: vec![Statement::Expr(ExpressionStatement {
                    loc: &b.location,
                    expression: Expression::Idx(Box::new(IndexExpression {
                        loc: &b.location,
                        type_info: type_info(fresher),
                        array: Expression::Call(Box::new(CallExpression {
                            loc: &b.location,
                            type_info: type_info(fresher),
                            pipe: None,
                            callee: Expression::Idt(IdentifierExpression {
                                loc: &b.location,
                                type_info: type_info(fresher),
                                name: "f".to_string(),
                            }),
                            arguments: ObjectExpression {
                                loc: &b.location,
                                type_info: type_info(fresher),
                                with: None,
                                properties: vec![],
                            },
                        })),
                        index: Expression::Int(IntegerLiteral {
                            loc: &b.location,
                            type_info: type_info(fresher),
                            value: 3,
                        }),
                    })),
                })],
            }],
        };
        let got = analyze_package(&pkg, fresher).unwrap();
        assert_eq!(want, got);
    }

    #[test]
    fn test_analyze_nested_member_expression() {
        let b = ast::BaseNode::default();
        let fresher = &mut TestFresher {};
        let pkg = ast::Package {
            base: b.clone(),
            path: "path".to_string(),
            package: "main".to_string(),
            files: vec![ast::File {
                base: b.clone(),
                name: "foo.flux".to_string(),
                package: None,
                imports: Vec::new(),
                body: vec![ast::Statement::Expr(ast::ExpressionStatement {
                    base: b.clone(),
                    expression: ast::Expression::Mem(Box::new(ast::MemberExpression {
                        base: b.clone(),
                        object: ast::Expression::Mem(Box::new(ast::MemberExpression {
                            base: b.clone(),
                            object: ast::Expression::Idt(ast::Identifier {
                                base: b.clone(),
                                name: "a".to_string(),
                            }),
                            property: ast::PropertyKey::Identifier(ast::Identifier {
                                base: b.clone(),
                                name: "b".to_string(),
                            }),
                        })),
                        property: ast::PropertyKey::Identifier(ast::Identifier {
                            base: b.clone(),
                            name: "c".to_string(),
                        }),
                    })),
                })],
            }],
        };
        let want = Package {
            loc: &b.location,
            package: "main".to_string(),
            files: vec![File {
                loc: &b.location,
                package: None,
                imports: Vec::new(),
                body: vec![Statement::Expr(ExpressionStatement {
                    loc: &b.location,
                    expression: Expression::Mem(Box::new(MemberExpression {
                        loc: &b.location,
                        type_info: type_info(fresher),
                        object: Expression::Mem(Box::new(MemberExpression {
                            loc: &b.location,
                            type_info: type_info(fresher),
                            object: Expression::Idt(IdentifierExpression {
                                loc: &b.location,
                                type_info: type_info(fresher),
                                name: "a".to_string(),
                            }),
                            property: "b".to_string(),
                        })),
                        property: "c".to_string(),
                    })),
                })],
            }],
        };
        let got = analyze_package(&pkg, fresher).unwrap();
        assert_eq!(want, got);
    }

    #[test]
    fn test_analyze_member_with_call_expression() {
        let b = ast::BaseNode::default();
        let fresher = &mut TestFresher {};
        let pkg = ast::Package {
            base: b.clone(),
            path: "path".to_string(),
            package: "main".to_string(),
            files: vec![ast::File {
                base: b.clone(),
                name: "foo.flux".to_string(),
                package: None,
                imports: Vec::new(),
                body: vec![ast::Statement::Expr(ast::ExpressionStatement {
                    base: b.clone(),
                    expression: ast::Expression::Mem(Box::new(ast::MemberExpression {
                        base: b.clone(),
                        object: ast::Expression::Call(Box::new(ast::CallExpression {
                            base: b.clone(),
                            callee: ast::Expression::Mem(Box::new(ast::MemberExpression {
                                base: b.clone(),
                                object: ast::Expression::Idt(ast::Identifier {
                                    base: b.clone(),
                                    name: "a".to_string(),
                                }),
                                property: ast::PropertyKey::Identifier(ast::Identifier {
                                    base: b.clone(),
                                    name: "b".to_string(),
                                }),
                            })),
                            arguments: vec![],
                        })),
                        property: ast::PropertyKey::Identifier(ast::Identifier {
                            base: b.clone(),
                            name: "c".to_string(),
                        }),
                    })),
                })],
            }],
        };
        let want = Package {
            loc: &b.location,
            package: "main".to_string(),
            files: vec![File {
                loc: &b.location,
                package: None,
                imports: Vec::new(),
                body: vec![Statement::Expr(ExpressionStatement {
                    loc: &b.location,
                    expression: Expression::Mem(Box::new(MemberExpression {
                        loc: &b.location,
                        type_info: type_info(fresher),
                        object: Expression::Call(Box::new(CallExpression {
                            loc: &b.location,
                            type_info: type_info(fresher),
                            pipe: None,
                            callee: Expression::Mem(Box::new(MemberExpression {
                                loc: &b.location,
                                type_info: type_info(fresher),
                                object: Expression::Idt(IdentifierExpression {
                                    loc: &b.location,
                                    type_info: type_info(fresher),
                                    name: "a".to_string(),
                                }),
                                property: "b".to_string(),
                            })),
                            arguments: ObjectExpression {
                                loc: &b.location,
                                type_info: type_info(fresher),
                                with: None,
                                properties: vec![],
                            },
                        })),
                        property: "c".to_string(),
                    })),
                })],
            }],
        };
        let got = analyze_package(&pkg, fresher).unwrap();
        assert_eq!(want, got);
    }
}
