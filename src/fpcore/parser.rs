//! Parser for FPCore.

use std::str::FromStr;

use calyx_utils::{FileIdx, GPosIdx, GlobalPositionTable};
use pest::error::{Error, ErrorVariant};
use pest_consume::{match_nodes, Parser};

use super::{ast, metadata};

#[derive(Parser)]
#[grammar = "fpcore/syntax.pest"]
pub struct FPCoreParser;

impl FPCoreParser {
    pub fn parse_file(
        name: String,
        src: String,
    ) -> Result<Vec<ast::FPCore>, Box<Error<Rule>>> {
        let file = GlobalPositionTable::as_mut().add_file(name, src);
        let src = GlobalPositionTable::as_ref().get_source(file);

        let nodes = FPCoreParser::parse_with_userdata(Rule::file, src, file)?;

        FPCoreParser::file(nodes.single()?).map_err(Box::new)
    }
}

type ParseResult<T> = Result<T, Error<Rule>>;
type Node<'i> = pest_consume::Node<'i, Rule, FileIdx>;

#[pest_consume::parser]
impl FPCoreParser {
    fn EOI(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn file(input: Node) -> ParseResult<Vec<ast::FPCore>> {
        Ok(match_nodes!(input.into_children();
            [fpcore(defs).., EOI(_)] => defs.collect(),
        ))
    }

    fn fpcore(input: Node) -> ParseResult<ast::FPCore> {
        Ok(match_nodes!(input.into_children();
            [symbol_opt(name), argument_list(args), property(props).., expr(body)] => {
                ast::FPCore {
                    name,
                    args,
                    props: props.collect(),
                    body,
                }
            },
        ))
    }

    fn symbol_opt(input: Node) -> ParseResult<Option<ast::Symbol>> {
        Ok(match_nodes!(input.into_children();
            [symbol(sym)] => Some(sym),
            [] => None,
        ))
    }

    fn argument_list(input: Node) -> ParseResult<Vec<ast::Argument>> {
        Ok(match_nodes!(input.into_children();
            [argument(args)..] => args.collect(),
        ))
    }

    fn dimension(input: Node) -> ParseResult<ast::Dimension> {
        Ok(match_nodes!(input.into_children();
            [number(num)] => ast::Dimension::Num(num),
            [symbol(sym)] => ast::Dimension::Id(sym),
        ))
    }

    fn argument(input: Node) -> ParseResult<ast::Argument> {
        Ok(match_nodes!(input.into_children();
            [symbol(var)] => ast::Argument {
                var,
                props: Vec::new(),
                dims: Vec::new(),
                uid: ast::NodeId::new(),
            },
            [annotation(props), symbol(var), dimension(dims)..] => {
                ast::Argument {
                    var,
                    props,
                    dims: dims.collect(),
                    uid: ast::NodeId::new(),
                }
            },
            [symbol(var), dimension(dims)..] => ast::Argument {
                var,
                props: Vec::new(),
                dims: dims.collect(),
                uid: ast::NodeId::new(),
            },
        ))
    }

    fn binding(input: Node) -> ParseResult<ast::Binding> {
        Ok(match_nodes!(input.into_children();
            [symbol(var), expr(expr)] => ast::Binding { var, expr },
        ))
    }

    fn mut_var(input: Node) -> ParseResult<ast::MutableVar> {
        Ok(match_nodes!(input.into_children();
            [symbol(var), expr(init), expr(update)] => {
                ast::MutableVar { var, init, update }
            },
        ))
    }

    fn index_var(input: Node) -> ParseResult<ast::InductionVar> {
        Ok(match_nodes!(input.into_children();
            [symbol(var), expr(size)] => ast::InductionVar { var, size },
        ))
    }

    fn mut_vars(input: Node) -> ParseResult<Vec<ast::MutableVar>> {
        Ok(match_nodes!(input.into_children();
            [mut_var(vars)..] => vars.collect(),
        ))
    }

    fn index_vars(input: Node) -> ParseResult<Vec<ast::InductionVar>> {
        Ok(match_nodes!(input.into_children();
            [index_var(vars)..] => vars.collect(),
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

    fn bang_kwd(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn digits_kwd(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn expr(input: Node) -> ParseResult<ast::Expression> {
        let span = intern_span(&input);

        let kind = match_nodes!(input.into_children();
            [number(num)] => ast::ExprKind::Num(num),
            [constant(constant)] => ast::ExprKind::Const(constant),
            [symbol(sym)] => ast::ExprKind::Id(sym),
            [operation(op)] => op,
            [if_kwd(_), expr(cond), expr(true_branch), expr(false_branch)] => {
                ast::ExprKind::If {
                    cond: Box::new(cond),
                    true_branch: Box::new(true_branch),
                    false_branch: Box::new(false_branch),
                }
            },
            [let_kwd(sequential), binding(bindings).., expr(body)] => {
                ast::ExprKind::Let {
                    bindings: bindings.collect(),
                    body: Box::new(body),
                    sequential,
                }
            },
            [while_kwd(sequential), expr(cond), mut_vars(vars), expr(body)] => {
                ast::ExprKind::While {
                    cond: Box::new(cond),
                    vars,
                    body: Box::new(body),
                    sequential,
                }
            },
            [for_kwd(sequential), index_vars(indices), mut_vars(vars), expr(body)] => {
                ast::ExprKind::For {
                    indices,
                    vars,
                    body: Box::new(body),
                    sequential,
                }
            },
            [tensor_kwd(_), index_vars(indices), expr(body)] => {
                ast::ExprKind::Tensor {
                    indices,
                    body: Box::new(body),
                }
            },
            [tensor_star_kwd(_), index_vars(indices), mut_vars(vars), expr(body)] => {
                ast::ExprKind::TensorStar {
                    indices,
                    vars,
                    body: Box::new(body),
                }
            },
            [cast_kwd(_), expr(body)] => ast::ExprKind::Cast(Box::new(body)),
            [array_kwd(_), expr(elems)..] => {
                ast::ExprKind::Array(elems.collect())
            },
            [annotation(props), expr(body)] => ast::ExprKind::Annotation {
                props,
                body: Box::new(body),
            },
        );

        Ok(ast::Expression {
            kind,
            uid: ast::NodeId::new(),
            span,
        })
    }

    fn number(input: Node) -> ParseResult<ast::Number> {
        let span = intern_span(&input);

        let value = match_nodes!(input.into_children();
            [rational(value)] => value,
            [decnum(value)] => value,
            [hexnum(value)] => value,
            [digits_kwd(_), mantissa((sign, mantissa)), exponent(exponent), dec_digits(base)] => {
                ast::Rational::from_digits(sign, mantissa, exponent, base)
                    .unwrap()
            },
        );

        Ok(ast::Number { value, span })
    }

    fn annotation(input: Node) -> ParseResult<Vec<ast::Property>> {
        Ok(match_nodes!(input.into_children();
            [bang_kwd(_), property(props)..] => props.collect(),
        ))
    }

    fn operation(input: Node) -> ParseResult<ast::ExprKind> {
        Ok(match_nodes!(input.into_children();
            [minus(span), expr(arg)] => {
                let kind = ast::OpKind::Math(ast::MathOp::Neg);
                let args = vec![arg];

                ast::ExprKind::Op(ast::Operation { kind, span }, args)
            },
            [minus(span), expr(minuend), expr(subtrahend)] => {
                let kind = ast::OpKind::Math(ast::MathOp::Sub);
                let args = vec![minuend, subtrahend];

                ast::ExprKind::Op(ast::Operation { kind, span }, args)
            },
            [operator(op), expr(args)..] => {
                ast::ExprKind::Op(op, args.collect())
            },
        ))
    }

    fn name_kwd(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn description_kwd(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn cite_kwd(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn precision_kwd(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn round_kwd(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn overflow_kwd(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn pre_kwd(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn spec_kwd(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn alt_kwd(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn math_lib_kwd(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn example_kwd(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn domain_kwd(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn impl_kwd(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn property_name(input: Node) -> ParseResult<ast::Symbol> {
        Ok(match_nodes!(input.into_children();
            [symbol(name)] => name,
        ))
    }

    fn property(input: Node) -> ParseResult<ast::Property> {
        Ok(match_nodes!(input.into_children();
            [name_kwd(_), string(name)] => ast::Property::Name(name),
            [description_kwd(_), string(description)] => {
                ast::Property::Description(description)
            },
            [cite_kwd(_), symbol(symbols)..] => {
                ast::Property::Cite(symbols.collect())
            },
            [precision_kwd(_), precision(precision)] => {
                ast::Property::Precision(precision)
            },
            [round_kwd(_), rounding(round)] => {
                ast::Property::Round(round.parse().unwrap())
            },
            [overflow_kwd(_), overflow(overflow)] => {
                ast::Property::Overflow(overflow.parse().unwrap())
            },
            [pre_kwd(_), expr(pre)] => ast::Property::Pre(pre),
            [spec_kwd(_), expr(spec)] => ast::Property::Spec(spec),
            [alt_kwd(_), expr(alt)] => ast::Property::Alt(alt),
            [math_lib_kwd(_), symbol(lib)] => ast::Property::MathLib(lib),
            [example_kwd(_), binding(bindings)..] => {
                ast::Property::Example(bindings.collect())
            },
            [domain_kwd(_), number(left), number(right)] => {
                ast::Property::CalyxDomain(metadata::CalyxDomain {
                    left,
                    right,
                })
            },
            [impl_kwd(_), strategy(strategy)] => {
                ast::Property::CalyxImpl(strategy)
            },
            [property_name(name), data(data)] => {
                ast::Property::Unknown(name, data)
            },
        ))
    }

    fn data(input: Node) -> ParseResult<metadata::Data> {
        Ok(match_nodes!(input.into_children();
            [symbol(sym)] => metadata::Data::Symbol(sym),
            [number(num)] => metadata::Data::Num(num),
            [string(s)] => metadata::Data::Str(s),
            [data(data)..] => metadata::Data::List(data.collect()),
        ))
    }

    fn float(input: Node) -> ParseResult<(u32, u32)> {
        Ok(match_nodes!(input.into_children();
            [e, nbits] => (parse_node(&e)?, parse_node(&nbits)?),
        ))
    }

    fn posit(input: Node) -> ParseResult<(u32, u32)> {
        Ok(match_nodes!(input.into_children();
            [es, nbits] => (parse_node(&es)?, parse_node(&nbits)?),
        ))
    }

    fn fixed(input: Node) -> ParseResult<(i32, u32)> {
        Ok(match_nodes!(input.into_children();
            [scale, nbits] => (parse_node(&scale)?, parse_node(&nbits)?),
        ))
    }

    fn precision_shorthand(input: Node) -> ParseResult<&str> {
        Ok(input.as_str())
    }

    fn precision(input: Node) -> ParseResult<metadata::Precision> {
        Ok(match_nodes!(input.into_children();
            [float((e, nbits))] => metadata::Precision::Float { e, nbits },
            [posit((es, nbits))] => metadata::Precision::Posit { es, nbits },
            [fixed((scale, nbits))] => {
                metadata::Precision::Fixed { scale, nbits }
            },
            [precision_shorthand(s)] => {
                metadata::Precision::from_shorthand(s).unwrap()
            },
        ))
    }

    fn rounding(input: Node) -> ParseResult<&str> {
        Ok(input.as_str())
    }

    fn overflow(input: Node) -> ParseResult<&str> {
        Ok(input.as_str())
    }

    fn lut(input: Node) -> ParseResult<u32> {
        match_nodes!(input.into_children();
            [size] => parse_node(&size),
        )
    }

    fn poly(input: Node) -> ParseResult<(u32, u32)> {
        Ok(match_nodes!(input.into_children();
            [degree, size] => (parse_node(&degree)?, parse_node(&size)?),
        ))
    }

    fn strategy(input: Node) -> ParseResult<metadata::CalyxImpl> {
        Ok(match_nodes!(input.into_children();
            [lut(lut_size)] => metadata::CalyxImpl::Lut { lut_size },
            [poly((degree, lut_size))] => {
                metadata::CalyxImpl::Poly { degree, lut_size }
            },
        ))
    }

    fn pm_opt(input: Node) -> ParseResult<ast::Sign> {
        Ok(match input.as_str() {
            "-" => ast::Sign::Neg,
            _ => ast::Sign::Pos,
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
            [pm_opt(sign), dec_digits(numerator), nonzero(denominator)] => {
                ast::Rational::from_ratio_str(sign, numerator, denominator, 10)
                    .unwrap()
            },
        ))
    }

    fn dec_mantissa(input: Node) -> ParseResult<(&str, &str)> {
        Ok(match_nodes!(input.into_children();
            [dec_digits(integer)] => (integer, "0"),
            [dec_digits(integer), dot(_), dec_digits(fraction)] => {
                (integer, fraction)
            },
            [dot(_), dec_digits(fraction)] => ("0", fraction),
        ))
    }

    fn hex_mantissa(input: Node) -> ParseResult<(&str, &str)> {
        Ok(match_nodes!(input.into_children();
            [hex_digits(integer)] => (integer, "0"),
            [hex_digits(integer), dot(_), hex_digits(fraction)] => {
                (integer, fraction)
            },
            [dot(_), hex_digits(fraction)] => ("0", fraction),
        ))
    }

    fn mantissa(input: Node) -> ParseResult<(ast::Sign, &str)> {
        Ok(match_nodes!(input.into_children();
            [pm_opt(sign), dec_digits(digits)] => (sign, digits),
        ))
    }

    fn exponent(input: Node) -> ParseResult<&str> {
        Ok(input.as_str())
    }

    fn decnum(input: Node) -> ParseResult<ast::Rational> {
        Ok(match_nodes!(input.into_children();
            [pm_opt(sign), dec_mantissa((integer, fraction)), exponent(exponent)] => {
                ast::Rational::from_scientific_str(
                    sign, integer, fraction, 10, 10, exponent,
                )
                .unwrap()
            },
            [pm_opt(sign), dec_mantissa((integer, fraction))] => {
                ast::Rational::from_fixed_point_str(sign, integer, fraction, 10)
                    .unwrap()
            },
        ))
    }

    fn hexnum(input: Node) -> ParseResult<ast::Rational> {
        Ok(match_nodes!(input.into_children();
            [pm_opt(sign), hex_mantissa((integer, fraction)), exponent(exponent)] => {
                ast::Rational::from_scientific_str(
                    sign, integer, fraction, 16, 2, exponent,
                )
                .unwrap()
            },
            [pm_opt(sign), hex_mantissa((integer, fraction))] => {
                ast::Rational::from_fixed_point_str(sign, integer, fraction, 16)
                    .unwrap()
            },
        ))
    }

    fn symbol(input: Node) -> ParseResult<ast::Symbol> {
        Ok(ast::Symbol {
            id: input.as_str().into(),
            span: intern_span(&input),
        })
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

    fn minus(input: Node) -> ParseResult<ast::Span> {
        Ok(intern_span(&input))
    }

    fn operator(input: Node) -> ParseResult<ast::Operation> {
        let span = intern_span(&input);

        let kind = match_nodes!(input.into_children();
            [mathematical_op(name)] => ast::OpKind::Math(name.parse().unwrap()),
            [testing_op(name)] => ast::OpKind::Test(name.parse().unwrap()),
            [tensor_op(name)] => ast::OpKind::Tensor(name.parse().unwrap()),
        );

        Ok(ast::Operation { kind, span })
    }

    fn mathematical_const(input: Node) -> ParseResult<&str> {
        Ok(input.as_str())
    }

    fn boolean_const(input: Node) -> ParseResult<&str> {
        Ok(input.as_str())
    }

    fn constant(input: Node) -> ParseResult<ast::Constant> {
        Ok(match_nodes!(input.into_children();
            [mathematical_const(name)] => {
                ast::Constant::Math(name.parse().unwrap())
            },
            [boolean_const(name)] => ast::Constant::Bool(match name {
                "TRUE" => true,
                "FALSE" => false,
                _ => unreachable!(),
            }),
        ))
    }
}

#[allow(clippy::result_large_err)]
fn parse_node<T>(input: &Node) -> ParseResult<T>
where
    T: FromStr,
    T::Err: ToString,
{
    input.as_str().parse().map_err(|err: T::Err| {
        Error::new_from_span(
            ErrorVariant::CustomError {
                message: err.to_string(),
            },
            input.as_span(),
        )
    })
}

fn intern_span(input: &Node) -> ast::Span {
    let span = input.as_span();

    let pos = GlobalPositionTable::as_mut().add_pos(
        *input.user_data(),
        span.start(),
        span.end(),
    );

    ast::Span(GPosIdx(pos))
}
