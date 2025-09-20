use std::fmt::Write;

use crate::parser::Expr;

pub fn display(e: &Expr) {
    let result = fmt_expr(e, 0);
    println!("{result}");
}

fn fmt_binop(left: &Expr, right: &Expr, op: &str, depth: usize) -> String {
    format!(
        "({} {} {})",
        fmt_expr(left, depth),
        op,
        fmt_expr(right, depth)
    )
}

fn fmt_expr(e: &Expr, depth: usize) -> String {
    match e {
        Expr::Assign(l, r) => fmt_binop(l, r, "=", depth),
        Expr::AddAssign(l, r) => fmt_binop(l, r, "+=", depth),
        Expr::SubtractAssign(l, r) => fmt_binop(l, r, "-=", depth),
        Expr::MultiplyAssign(l, r) => fmt_binop(l, r, "*=", depth),
        Expr::DivideAssign(l, r) => fmt_binop(l, r, "/=", depth),
        Expr::Literal(l) => l.to_string(),
        Expr::Call(l, r) => {
            let mut result = fmt_expr(l, depth);
            result.push('(');
            let len = r.len();
            for (i, e) in r.iter().enumerate() {
                result.push_str(&fmt_expr(e, depth));
                if i + 1 != len {
                    result.push_str(", ");
                }
            }
            result.push(')');
            result
        }
        Expr::If(c, t, f) => {
            let mut result = format!("if ({}) ({})", fmt_expr(c, depth), fmt_expr(t, depth));
            if let Some(f) = f {
                let _ = write!(result, " else ({})", fmt_expr(f, depth));
            }
            result
        }
        Expr::Return(l) => format!("return ({})", fmt_expr(l, depth)),
        Expr::Block(b) => {
            let mut result = String::new();
            let len = b.exprs.len();
            for (i, expr) in b.exprs.iter().enumerate() {
                result.push_str(&"  ".repeat(depth));
                result.push_str(&fmt_expr(expr, depth + 1));
                if depth != 0 || i + 1 != len {
                    result.push('\n');
                }
            }
            if depth != 0 {
                result = format!("{{\n{result}{}}}", "  ".repeat(depth - 1));
            }
            result
        }
        Expr::Func(a, e) => format!(
            "(func({}) ({}))",
            a.iter()
                .map(|e| fmt_expr(e, depth))
                .collect::<Vec<_>>()
                .join(", "),
            fmt_expr(e, depth)
        ),
        Expr::Negate(l) => format!("(-{})", fmt_expr(l, depth)),
        Expr::Not(l) => format!("(!{})", fmt_expr(l, depth)),
        Expr::EqualTo(l, r) => fmt_binop(l, r, "==", depth),
        Expr::NotEqualTo(l, r) => fmt_binop(l, r, "!=", depth),
        Expr::And(l, r) => fmt_binop(l, r, "&&", depth),
        Expr::Or(l, r) => fmt_binop(l, r, "||", depth),
        Expr::LessThan(l, r) => fmt_binop(l, r, "<", depth),
        Expr::LessThanOrEqualTo(l, r) => fmt_binop(l, r, "<=", depth),
        Expr::GreaterThan(l, r) => fmt_binop(l, r, ">", depth),
        Expr::GreaterThanOrEqualTo(l, r) => fmt_binop(l, r, ">=", depth),
        Expr::Add(l, r) => fmt_binop(l, r, "+", depth),
        Expr::Subtract(l, r) => fmt_binop(l, r, "-", depth),
        Expr::Multiply(l, r) => fmt_binop(l, r, "*", depth),
        Expr::Divide(l, r) => fmt_binop(l, r, "/", depth),
        Expr::Exponent(l, r) => fmt_binop(l, r, "**", depth),
        Expr::Modulo(l, r) => fmt_binop(l, r, "%", depth),
    }
}
