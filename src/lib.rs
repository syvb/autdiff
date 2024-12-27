use std::{any::Any, fmt::Debug};

#[derive(Debug)]
pub struct Expr {
    pub kind: Box<dyn ExprKind>,
}
pub trait ExprKind: PartialEq<Expr> + Debug + Any {
    fn simplify(&mut self) {}
}

pub mod ops {
    use super::*;

    macro_rules! expr_op_impl {
        ($op:tt, $f:ident) => {
            #[derive(Debug)]
            struct $op(Expr, Expr);
            impl ExprKind for $op {}
            impl PartialEq<Expr> for $op {
                fn eq(&self, rhs: &Expr) -> bool {
                    if let Some(other) = Box::<(dyn Any + 'static)>::downcast(rhs.kind) {
                        self.0 == other.0 && self.1 == other.1
                    } else {
                        false
                    }
                }
            }

            impl std::ops::$op for Expr {
                type Output = Self;
                fn $f(self, rhs: Self) -> Self::Output {
                    Expr {
                        kind: Box::new($op(self, rhs)),
                    }
                }
            }
        };
    }
    expr_op_impl!(Add, add);
    expr_op_impl!(Sub, sub);
    expr_op_impl!(Div, div);
    expr_op_impl!(Mul, mul);
}


#[cfg(test)]
mod tests {
    use super::*;

}
