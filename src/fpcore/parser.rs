//! Parser for FPCore.

use super::ast;
use pest_consume::{match_nodes, Error, Parser};

#[derive(Parser)]
#[grammar = "fpcore/syntax.pest"]
pub struct FPCoreParser;

impl FPCoreParser {
    pub fn parse_file(src: &str) -> ParseResult<Vec<ast::BenchmarkDef>> {
        let nodes = FPCoreParser::parse(Rule::file, src)?;
        FPCoreParser::file(nodes.single()?)
    }
}

type ParseResult<T> = Result<T, Error<Rule>>;
type Node<'i> = pest_consume::Node<'i, Rule, ()>;

#[pest_consume::parser]
impl FPCoreParser {
    fn EOI(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn file(input: Node) -> ParseResult<Vec<ast::BenchmarkDef>> {
        Ok(match_nodes!(input.into_children();
            [FPCore(benchmarks).., EOI(_)] => benchmarks.collect(),
        ))
    }

    fn FPCore(input: Node) -> ParseResult<ast::BenchmarkDef> {
        Ok(match_nodes!(input.into_children();
            [symbol_opt(name), argument_list(args), property(props).., expr(body)] => ast::BenchmarkDef {
                name,
                args,
                props: props.collect(),
                body,
            },
        ))
    }

    fn symbol_opt(input: Node) -> ParseResult<Option<ast::Symbol>> {
        Ok(match_nodes!(input.into_children();
            [symbol(id)] => Some(id),
            [] => None,
        ))
    }

    fn argument_list(input: Node) -> ParseResult<Vec<ast::ArgumentDef>> {
        Ok(match_nodes!(input.into_children();
            [argument(args)..] => args.collect(),
        ))
    }

    fn dimension(input: Node) -> ParseResult<ast::Dimension> {
        Ok(match_nodes!(input.into_children();
            [number(num)] => ast::Dimension::Num(num),
            [symbol(id)] => ast::Dimension::Id(id),
        ))
    }

    fn annotation(input: Node) -> ParseResult<Vec<ast::Property>> {
        Ok(match_nodes!(input.into_children();
            [property(props)..] => props.collect(),
        ))
    }

    fn argument(input: Node) -> ParseResult<ast::ArgumentDef> {
        Ok(match_nodes!(input.into_children();
            [symbol(var)] => ast::ArgumentDef::Id(var),
            [annotation(props), symbol(var), dimension(dims)..] => ast::ArgumentDef::Annotated {
                props,
                var,
                dims: dims.collect(),
            },
            [symbol(var), dimension(dims)..] => ast::ArgumentDef::Sized {
                var,
                dims: dims.collect(),
            },
        ))
    }

    fn binder(input: Node) -> ParseResult<ast::Binder> {
        Ok(match_nodes!(input.into_children();
            [symbol(var), expr(expr)] => ast::Binder { var, expr },
        ))
    }

    fn update_rule(input: Node) -> ParseResult<ast::UpdateRule> {
        Ok(match_nodes!(input.into_children();
            [symbol(var), expr(init), expr(update)] => ast::UpdateRule {
                var, init, update
            },
        ))
    }

    fn condition(input: Node) -> ParseResult<ast::Condition> {
        Ok(match_nodes!(input.into_children();
            [symbol(var), expr(val)] => ast::Condition { var, val },
        ))
    }

    fn rules(input: Node) -> ParseResult<Vec<ast::UpdateRule>> {
        Ok(match_nodes!(input.into_children();
            [update_rule(rules)..] => rules.collect(),
        ))
    }

    fn conditions(input: Node) -> ParseResult<Vec<ast::Condition>> {
        Ok(match_nodes!(input.into_children();
            [condition(conditions)..] => conditions.collect(),
        ))
    }

    fn if_kwd(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn let_kwd(input: Node) -> ParseResult<bool> {
        Ok(input.as_str().ends_with('*'))
    }

    fn while_kwd(input: Node) -> ParseResult<bool> {
        Ok(input.as_str().ends_with('*'))
    }

    fn for_kwd(input: Node) -> ParseResult<bool> {
        Ok(input.as_str().ends_with('*'))
    }

    fn tensor_kwd(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn tensor_star_kwd(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn cast_kwd(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn array_kwd(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn expr(input: Node) -> ParseResult<ast::Expression> {
        Ok(match_nodes!(input.into_children();
            [number(num)] => ast::Expression::Num(num),
            [constant(constant)] => ast::Expression::Const(constant),
            [symbol(id)] => ast::Expression::Id(id),
            [operation(op), expr(expressions)..] =>
                ast::Expression::Op(op, expressions.collect()),
            [if_kwd(_), expr(cond), expr(if_true), expr(if_false)] => ast::Expression::If {
                cond: Box::new(cond),
                if_true: Box::new(if_true),
                if_false: Box::new(if_false),
            },
            [let_kwd(sequential), binder(binders).., expr(body)] => ast::Expression::Let {
                binders: binders.collect(),
                body: Box::new(body),
                sequential,
            },
            [while_kwd(sequential), expr(cond), rules(rules), expr(body)] => ast::Expression::While {
                cond: Box::new(cond),
                rules,
                body: Box::new(body),
                sequential,
            },
            [for_kwd(sequential), conditions(conditions), rules(rules), expr(body)] => ast::Expression::For {
                conditions,
                rules,
                body: Box::new(body),
                sequential,
            },
            [tensor_kwd(_), conditions(conditions), expr(body)] => ast::Expression::Tensor {
                conditions,
                body: Box::new(body),
            },
            [tensor_star_kwd(_), conditions(conditions), rules(rules), expr(body)] => ast::Expression::TensorStar {
                conditions,
                rules,
                body: Box::new(body),
            },
            [cast_kwd(_), expr(body)] =>
                ast::Expression::Cast(Box::new(body)),
            [array_kwd(_), expr(elems)..] =>
                ast::Expression::Array(elems.collect()),
            [annotation(props), expr(body)] => ast::Expression::Annotation {
                props,
                body: Box::new(body),
            },
        ))
    }

    fn number(input: Node) -> ParseResult<ast::Number> {
        Ok(match_nodes!(input.into_children();
            [rational(rational)] => ast::Number::Rational(rational),
            [decnum(rational)] => ast::Number::Rational(rational),
            [hexnum(rational)] => ast::Number::Rational(rational),
            [decnum(mantissa), decnum(exponent), decnum(base)] => ast::Number::Digits {
                mantissa, exponent, base
            },
        ))
    }

    fn property_name(input: Node) -> ParseResult<ast::Symbol> {
        Ok(match_nodes!(input.into_children();
            [symbol(name)] => name,
        ))
    }

    fn property(input: Node) -> ParseResult<ast::Property> {
        Ok(match_nodes!(input.into_children();
            [property_name(name), data(data)] => ast::Property {
                name, data,
            },
        ))
    }

    fn data(input: Node) -> ParseResult<ast::Data> {
        Ok(match_nodes!(input.into_children();
            [string(s)] => ast::Data::Str(s),
            [binder(binder)] => ast::Data::Binder(binder),
            [expr(expression)] => ast::Data::Expr(expression),
            [data(data)..] => ast::Data::List(data.collect()),
        ))
    }

    fn pm_opt(input: Node) -> ParseResult<ast::Sign> {
        Ok(match input.as_str() {
            "-" => ast::Sign::Neg,
            _ => ast::Sign::NonNeg,
        })
    }

    fn dot(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn dec_digits(input: Node) -> ParseResult<&str> {
        Ok(input.as_str())
    }

    fn hex_digits(input: Node) -> ParseResult<&str> {
        Ok(input.as_str())
    }

    fn nonzero(input: Node) -> ParseResult<&str> {
        Ok(input.as_str())
    }

    fn rational(input: Node) -> ParseResult<ast::Rational> {
        Ok(match_nodes!(input.into_children();
            [pm_opt(sign), dec_digits(numerator), nonzero(denominator)] =>
                ast::Rational::from_ratio_str(sign, numerator, denominator, 10).unwrap(),
        ))
    }

    fn dec_mantissa(input: Node) -> ParseResult<(&str, &str)> {
        Ok(match_nodes!(input.into_children();
            [dec_digits(integer)] =>
                (integer, "0"),
            [dec_digits(integer), dot(_), dec_digits(fraction)] =>
                (integer, fraction),
            [dot(_), dec_digits(fraction)] =>
                ("0", fraction),
        ))
    }

    fn hex_mantissa(input: Node) -> ParseResult<(&str, &str)> {
        Ok(match_nodes!(input.into_children();
            [hex_digits(integer)] =>
                (integer, "0"),
            [hex_digits(integer), dot(_), hex_digits(fraction)] =>
                (integer, fraction),
            [dot(_), hex_digits(fraction)] =>
                ("0", fraction),
        ))
    }

    fn exponent(input: Node) -> ParseResult<&str> {
        Ok(input.as_str())
    }

    fn decnum(input: Node) -> ParseResult<ast::Rational> {
        Ok(match_nodes!(input.into_children();
            [pm_opt(sign), dec_mantissa((integer, fraction)), exponent(exponent)] =>
                ast::Rational::from_scientific_str(sign, integer, fraction, 10, 10, exponent).unwrap(),
            [pm_opt(sign), dec_mantissa((integer, fraction))] =>
                ast::Rational::from_fixed_point_str(sign, integer, fraction, 10).unwrap(),
        ))
    }

    fn hexnum(input: Node) -> ParseResult<ast::Rational> {
        Ok(match_nodes!(input.into_children();
            [pm_opt(sign), hex_mantissa((integer, fraction)), exponent(exponent)] =>
                ast::Rational::from_scientific_str(sign, integer, fraction, 16, 2, exponent).unwrap(),
            [pm_opt(sign), hex_mantissa((integer, fraction))] =>
                ast::Rational::from_fixed_point_str(sign, integer, fraction, 16).unwrap(),
        ))
    }

    fn symbol(input: Node) -> ParseResult<ast::Symbol> {
        Ok(ast::Symbol(input.as_str().into()))
    }

    fn printable(input: Node) -> ParseResult<&str> {
        Ok(match input.as_str() {
            "\\\\" => "\\",
            "\\\"" => "\"",
            s => s,
        })
    }

    fn string(input: Node) -> ParseResult<String> {
        Ok(match_nodes!(input.into_children();
            [printable(c)..] => c.collect(),
        ))
    }

    fn mathematical_op(input: Node) -> ParseResult<&str> {
        Ok(input.as_str())
    }

    fn testing_op(input: Node) -> ParseResult<&str> {
        Ok(input.as_str())
    }

    fn tensor_op(input: Node) -> ParseResult<&str> {
        Ok(input.as_str())
    }

    fn operation(input: Node) -> ParseResult<ast::Operation> {
        Ok(match_nodes!(input.into_children();
            [mathematical_op(name)] => ast::Operation::Math(name.parse().unwrap()),
            [testing_op(name)] => ast::Operation::Test(name.parse().unwrap()),
            [tensor_op(name)] => ast::Operation::Tensor(name.parse().unwrap()),
        ))
    }

    fn mathematical_const(input: Node) -> ParseResult<&str> {
        Ok(input.as_str())
    }

    fn boolean_const(input: Node) -> ParseResult<&str> {
        Ok(input.as_str())
    }

    fn constant(input: Node) -> ParseResult<ast::Constant> {
        Ok(match_nodes!(input.into_children();
            [mathematical_const(name)] => ast::Constant::Math(name.parse().unwrap()),
            [boolean_const(name)] => ast::Constant::Bool(match name {
                "TRUE" => true,
                "FALSE" => false,
                _ => unreachable!(),
            }),
        ))
    }
}
