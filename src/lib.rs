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

#[derive(Debug, Clone)]
pub enum ExprKind {
    Integer(i64),

    // simple unary ops
    Neg(Expr),

    // simple binary ops
    Add(Expr, Expr),
    Sub(Expr, Expr),
    Div(Expr, Expr),
    Mul(Expr, Expr),
}

impl Expr {
    pub fn simplify(self) -> Self {
        macro_rules! ops {
            ($expr:expr , $($name:ident : $op:expr,)+) => {
                match *self.kind {
                    // already simple
                    ExprKind::Integer(_) | ExprKind::Div(_, _) => self,

                    // unary
                    ExprKind::Neg(inner) => {
                        let inner = inner.simplify();
                        if let ExprKind::Integer(n) = *inner.kind {
                            Expr {
                                kind: Box::new(ExprKind::Integer(-n)),
                            }
                        } else {
                            inner
                        }
                    }


                    $(
                        ExprKind::$name(x, y) => {
                            let x = x.simplify();
                            let y = y.simplify();
                            if let (ExprKind::Integer(x), ExprKind::Integer(y)) = (&*x.kind, &*y.kind) {
                                Expr {
                                    kind: Box::new(ExprKind::Integer(($op)(x,y))),
                                }
                            } else {
                                Expr {
                                    kind: Box::new(ExprKind::$name(x, y)),
                                }
                            }
                        }
                    )+
                }
            };
        }
        ops!(
            *self.kind,
            Add: |x, y| x + y,
            Sub: |x, y| x - y,
            Mul: |x, y| x * y,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
}
