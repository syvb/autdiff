use std::ops;

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
}
#[inline(always)]
pub fn n(n: f64) -> Expr {
    Expr::n(n)
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Number(f64),

    // simple binary ops
    Add(Expr, Expr),
    Mul(Expr, Expr),
    Pow(Expr, Expr),
}

impl Expr {
    pub fn simplify(self) -> Self {
        match *self.kind {
            ExprKind::Number(_) => self,
            ExprKind::Add(x, y) => {
                let x = x.simplify();
                let y = y.simplify();
                match (&*x.kind, &*y.kind) {
                    (ExprKind::Number(x), ExprKind::Number(0.))
                    | (ExprKind::Number(0.), ExprKind::Number(x)) => Expr {
                        kind: Box::new(ExprKind::Number(*x)),
                    },
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
                if let (ExprKind::Number(x), ExprKind::Number(y)) = (&*x.kind, &*y.kind) {
                    Expr {
                        kind: Box::new(ExprKind::Number((|x, y| x * y)(x, y))),
                    }
                } else {
                    Expr {
                        kind: Box::new(ExprKind::Mul(x, y)),
                    }
                }
            }
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
        // (10 + (1 * -1)) * (3^-1)
        panic!("{:#?}", expr.simplify());
    }
}
