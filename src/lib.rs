use std::ops;

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: Box<ExprKind>,
}
impl Expr {
    pub fn i(n: i64) -> Self {
        Expr {
            kind: Box::new(ExprKind::Integer(n)),
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
        self.pow(i(-1))
    }
}
#[inline(always)]
pub fn i(n: i64) -> Expr {
    Expr::i(n)
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Integer(i64),

    // simple binary ops
    Add(Expr, Expr),
    Mul(Expr, Expr),
    Pow(Expr, Expr),
}

impl Expr {
    pub fn simplify(self) -> Self {
        match *self.kind {
            ExprKind::Integer(_) => self,
            ExprKind::Add(x, y) => {
                let x = x.simplify();
                let y = y.simplify();
                match (&*x.kind, &*y.kind) {
                    (ExprKind::Integer(x), ExprKind::Integer(0))
                    | (ExprKind::Integer(0), ExprKind::Integer(x)) => Expr {
                        kind: Box::new(ExprKind::Integer(*x)),
                    },
                    (ExprKind::Integer(x), ExprKind::Integer(y)) => Expr {
                        kind: Box::new(ExprKind::Integer(x + y)),
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
                    (x, ExprKind::Integer(1)) => Expr {
                        kind: Box::new(x.clone()),
                    },
                    (_, ExprKind::Integer(0)) => i(1),
                    (ExprKind::Integer(0), _) => i(0),
                    (ExprKind::Integer(1), _) => i(1),
                    (ExprKind::Integer(x), ExprKind::Integer(y))
                        if *y > 0 && *y < u32::MAX as i64 =>
                    {
                        Expr {
                            kind: Box::new(ExprKind::Integer(x.pow(*y as u32))),
                        }
                    }
                    _ => Expr {
                        kind: Box::new(ExprKind::Pow(x, y)),
                    },
                }
            }
            ExprKind::Mul(x, y) => {
                let x = x.simplify();
                let y = y.simplify();
                if let (ExprKind::Integer(x), ExprKind::Integer(y)) = (&*x.kind, &*y.kind) {
                    Expr {
                        kind: Box::new(ExprKind::Integer((|x, y| x * y)(x, y))),
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
            kind: Box::new(ExprKind::Mul(self, i(-1))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn arithmetic() {
        let expr = (i(10) - i(1)) / i(3);
        // (10 + (1 * -1)) * (3^-1)
        panic!("{:#?}", expr);
    }
}
