use std::{fmt::Display, ops};

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: Box<ExprKind>,
}
impl Expr {
    pub fn n(n: f64) -> Self {
        Expr {
            kind: Box::new(ExprKind::Number(n)),
        }
    }
    pub fn v(name: &'static str) -> Self {
        Expr {
            kind: Box::new(ExprKind::Variable(name)),
        }
    }
}
impl Expr {
    pub fn pow(self, rhs: Self) -> Self {
        Expr {
            kind: Box::new(ExprKind::Pow(self, rhs)),
        }
    }
    pub fn recip(self) -> Self {
        self.pow(n(-1.))
    }
    pub fn ln(self) -> Self {
        Expr {
            kind: Box::new(ExprKind::Ln(self)),
        }
    }
}
#[inline(always)]
pub fn n(n: f64) -> Expr {
    Expr::n(n)
}
pub fn v(name: &'static str) -> Expr {
    Expr::v(name)
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Variable(&'static str),
    Number(f64),

    Add(Expr, Expr),
    Mul(Expr, Expr),
    Pow(Expr, Expr),
    Ln(Expr),
}

impl Expr {
    pub fn simplify(self) -> Self {
        match *self.kind {
            ExprKind::Number(_) | ExprKind::Variable(_) | ExprKind::Ln(_) => self,
            ExprKind::Add(x, y) => {
                let x = x.simplify();
                let y = y.simplify();
                match (&*x.kind, &*y.kind) {
                    (ExprKind::Number(x), ExprKind::Number(z))
                    | (ExprKind::Number(z), ExprKind::Number(x))
                        if z.abs() < 0.0001 =>
                    {
                        Expr {
                            kind: Box::new(ExprKind::Number(*x)),
                        }
                    }
                    (ExprKind::Number(x), ExprKind::Number(y)) => Expr {
                        kind: Box::new(ExprKind::Number(x + y)),
                    },
                    _ => Expr {
                        kind: Box::new(ExprKind::Add(x, y)),
                    },
                }
            }
            ExprKind::Pow(x, y) => {
                let x = x.simplify();
                let y = y.simplify();
                match (&*x.kind, &*y.kind) {
                    (x, ExprKind::Number(1.)) => Expr {
                        kind: Box::new(x.clone()),
                    },
                    (_, ExprKind::Number(0.)) => n(1.),
                    (ExprKind::Number(0.), _) => n(0.),
                    (ExprKind::Number(1.), _) => n(1.),
                    (ExprKind::Number(x), ExprKind::Number(y)) => Expr {
                        kind: Box::new(ExprKind::Number(x.powf(*y))),
                    },
                    _ => Expr {
                        kind: Box::new(ExprKind::Pow(x, y)),
                    },
                }
            }
            ExprKind::Mul(x, y) => {
                let x = x.simplify();
                let y = y.simplify();
                match (&*x.kind, &*y.kind) {
                    (_, ExprKind::Number(z)) | (ExprKind::Number(z), _) if z.abs() < 0.0001 => {
                        n(0.)
                    }
                    (ExprKind::Number(x), ExprKind::Number(y)) => Expr {
                        kind: Box::new(ExprKind::Number(x * y)),
                    },
                    _ => Expr {
                        kind: Box::new(ExprKind::Mul(x, y)),
                    },
                }
            }
        }
    }
    pub fn diff(self, to: &'static str) -> Self {
        match *self.kind {
            ExprKind::Variable(v) if v == to => n(1.),
            ExprKind::Variable(_) => n(0.),
            ExprKind::Number(_) => n(0.),
            ExprKind::Add(a, b) => a.diff(to) + b.diff(to),
            ExprKind::Mul(a, b) => a.clone().diff(to) * b.clone() + a * b.diff(to),
            ExprKind::Pow(a, b) => {
                a.clone().pow(b.clone() - n(1.))
                    * (a.clone().diff(to) * b.clone() + a.clone() * a.ln() * b.diff(to))
            }
            // ln(g(x)) = ln'(g(x))*g'(x) = 1/g(x) * g'(x)
            ExprKind::Ln(a) => a.clone().recip() * a.diff(to),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self.kind.clone() {
            ExprKind::Variable(v) => f.write_str(v),
            ExprKind::Number(n) => f.write_fmt(format_args!("{}", n)),
            ExprKind::Ln(expr) => f.write_fmt(format_args!("ln({})", expr)),
            ExprKind::Add(a, b) => f.write_fmt(format_args!("({})+({})", a, b)),
            ExprKind::Mul(a, b) => f.write_fmt(format_args!("({})*({})", a, b)),
            ExprKind::Pow(a, b) => f.write_fmt(format_args!("({})^({})", a, b)),
        }
    }
}

macro_rules! expr_std_op {
    ($op:ident, $oplower:ident) => {
        impl ops::$op for Expr {
            type Output = Self;
            fn $oplower(self, rhs: Self) -> Self::Output {
                Expr {
                    kind: Box::new(ExprKind::$op(self, rhs)),
                }
            }
        }
    };
}
expr_std_op!(Add, add);
impl ops::Sub for Expr {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        Expr {
            kind: Box::new(ExprKind::Add(self, -rhs)),
        }
    }
}
expr_std_op!(Mul, mul);
impl ops::Div for Expr {
    type Output = Self;
    fn div(self, rhs: Self) -> Self::Output {
        Expr {
            kind: Box::new(ExprKind::Mul(self, rhs.recip())),
        }
    }
}
impl ops::Neg for Expr {
    type Output = Self;
    fn neg(self) -> Self::Output {
        Expr {
            kind: Box::new(ExprKind::Mul(self, n(-1.))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn arithmetic() {
        let expr = (n(10.) - n(1.)) / n(4.);
        panic!("{}", expr);
    }

    #[test]
    fn diff() {
        let expr = v("x").pow(n(-3.));
        panic!(
            "{}, simple: {}",
            expr.clone().diff("x"),
            expr.diff("x").simplify()
        );
    }
}
