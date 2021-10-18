use crate::context::Context;
use cel_parser::{ArithmeticOp, Atom, Expression, Member, RelationOp, UnaryOp};
use core::ops;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::convert::TryInto;
use std::rc::Rc;
use crate::InterpreterError;

#[derive(Debug, PartialEq, Clone)]
pub struct CelMap {
    pub map: Rc<HashMap<CelKey, CelType>>,
}


impl PartialOrd for CelMap {
    fn partial_cmp(&self, _other: &Self) -> Option<Ordering> {
        None
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub enum CelKey {
    Int(i32),
    Uint(u32),
    Bool(bool),
    String(Rc<String>),
}

impl<'a> TryInto<CelKey> for CelType {
    type Error = InterpreterError;

    #[inline(always)]
    fn try_into(self) -> Result<CelKey, Self::Error> {
        match self {
            CelType::Int(v) => Ok(CelKey::Int(v)),
            CelType::UInt(v) => Ok(CelKey::Uint(v)),
            CelType::String(v) => Ok(CelKey::String(v)),
            CelType::Bool(v) => Ok(CelKey::Bool(v)),
            _ => Err(InterpreterError::TypeTranslationError),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum CelType {
    List(Rc<[CelType]>),
    Map(CelMap),

    Function(Rc<String>, Option<Box<CelType>>),

    // Atoms
    Int(i32),
    UInt(u32),
    Float(f64),
    String(Rc<String>),
    Bytes(Rc<Vec<u8>>),
    Bool(bool),
    Null,
}

impl<'a> CelType {
    #[inline(always)]
    pub fn resolve(expr: &'a Expression, ctx: &Context) -> Result<CelType, InterpreterError> {
        let cel_type = match expr {
            Expression::Atom(atom) => atom.into(),
            Expression::Arithmetic(left, op, right) => {
                let left = CelType::resolve(left, ctx)?;
                let right = CelType::resolve(right, ctx)?;

                match op {
                    ArithmeticOp::Add => (left + right)?,
                    ArithmeticOp::Subtract => (left - right)?,
                    ArithmeticOp::Divide => (left / right)?,
                    ArithmeticOp::Multiply => (left * right)?,
                    ArithmeticOp::Modulus => (left % right)?,
                }
            }
            Expression::Relation(left, op, right) => {
                let left = CelType::resolve(left, ctx)?;
                let right = CelType::resolve(right, ctx)?;
                let res = match op {
                    RelationOp::LessThan => left < right,
                    RelationOp::LessThanEq => left <= right,
                    RelationOp::GreaterThan => left > right,
                    RelationOp::GreaterThanEq => left >= right,
                    RelationOp::Equals => right.eq(&left),
                    RelationOp::NotEquals => right.ne(&left),
                    RelationOp::In => match (left, right) {
                        (CelType::String(l), CelType::String(r)) => r.contains(l.as_str()),
                        (any, CelType::List(v)) => v.contains(&any),
                        (any, CelType::Map(m)) => m.map.contains_key(&any.try_into()?),
                        _ => return Err(InterpreterError::OperandMismatch)
                    },
                };
                CelType::Bool(res)
            }
            Expression::Ternary(cond, left, right) => {
                let cond = CelType::resolve(cond, ctx)?;
                if cond.to_bool() {
                    CelType::resolve(left, ctx)?
                } else {
                    CelType::resolve(right, ctx)?
                }
            }
            Expression::Or(left, right) => {
                let left = CelType::resolve(left, ctx)?;
                if left.to_bool() {
                    left
                } else {
                    CelType::resolve(right, ctx)?
                }
            }
            Expression::And(left, right) => {
                let left = CelType::resolve(left, ctx)?;
                let right = CelType::resolve(right, ctx)?;
                CelType::Bool(left.to_bool() && right.to_bool())
            }
            Expression::Unary(op, expr) => {
                let expr = CelType::resolve(expr, ctx)?;
                match op {
                    UnaryOp::Not => CelType::Bool(!expr.to_bool()),
                    UnaryOp::DoubleNot => CelType::Bool(expr.to_bool()),
                    UnaryOp::Minus => match expr {
                        CelType::Int(i) => CelType::Int(-i),
                        CelType::Float(i) => CelType::Float(-i),
                        _ => return Err(InterpreterError::OperandMismatch),
                    },
                    UnaryOp::DoubleMinus => match expr {
                        CelType::Int(_) => expr,
                        CelType::UInt(_) => expr,
                        CelType::Float(_) => expr,
                        _ => return Err(InterpreterError::OperandMismatch),
                    },
                }
            }
            Expression::Member(left, right) => {
                let left = CelType::resolve(left, ctx)?;
                left.member(right, ctx)?
            }
            Expression::List(items) => {
                let list = items.iter().map(|i| CelType::resolve(i, ctx)).collect::<Result<Rc<[CelType]>,_>>()?;
                CelType::List(list)
            }
            Expression::Map(items) => {
                let map = items
                    .iter()
                    .map(|(k, v)| {
                        let key = CelType::resolve(k, ctx).and_then(|c| c.try_into())?;
                        let value = CelType::resolve(v, ctx)?;
                        Ok((key, value))
                    })
                    .collect::<Result<HashMap<CelKey, CelType>, _>>()?;
                    
                CelType::Map(CelMap { map: Rc::from(map) })
            }
            Expression::Ident(name) => {
                if ctx.functions.contains_key(name.as_str()) {
                    CelType::Function(name.clone(), None)
                } else if ctx.variables.contains_key(name.as_str()) {
                    ctx.variables.get(name.as_str()).unwrap().clone()
                } else {
                    return Err(InterpreterError::UnknownVariable{variable_name: name.as_ref().to_owned()})
                }
            }
        };

        Ok(cel_type)
    }

    // >> a(b)
    // Member(Ident("a"),
    //        FunctionCall([Ident("b")]))
    // >> a.b(c)
    // Member(Member(Ident("a"),
    //               Attribute("b")),
    //        FunctionCall([Ident("c")]))

    #[inline(always)]
    fn member(self, member: &Member, ctx: &Context) -> Result<CelType, InterpreterError> {
        let cel_type = match member {
            Member::Index(idx) => {
                let idx = CelType::resolve(idx, ctx)?;
                match (self, idx) {
                    (CelType::List(items), CelType::Int(idx)) => {
                        items.get(idx as usize).ok_or(InterpreterError::IndexBoundsError)?.clone()
                    }
                    _ => return Err(InterpreterError::OperandMismatch),
                }
            }
            Member::Fields(_) => return Err(InterpreterError::OperandMismatch),
            Member::Attribute(name) => {
                if ctx.functions.contains_key(name.as_str()) {
                    CelType::Function(name.clone(), Some(self.into()))
                } else {
                    return Err(InterpreterError::UnknownVariable{variable_name: (name.as_str()).to_owned()})
                }
            }
            Member::FunctionCall(args) => {
                if let CelType::Function(name, target) = self {
                    let func = ctx.functions.get(name.as_str()).ok_or(InterpreterError::UnknownVariable{variable_name: name.as_ref().to_owned() })?;
                    match target {
                        None => {
                            // Strange case, a function with no arguments...!
                            if args.is_empty() {
                                func(None, args, ctx)?
                            } else {
                                let first_arg = CelType::resolve(&args[0], ctx)?;
                                func(Some(&first_arg), &args[1..args.len()], ctx)?
                            }
                        }
                        Some(t) => func(Some(t.as_ref()), args, ctx)?,
                    }
                } else {
                    unreachable!("FunctionCall without CelType::Function - {:?}", self)
                }
            }
        };
        return Ok(cel_type);
    }

    #[inline(always)]
    fn to_bool(&self) -> bool {
        match self {
            CelType::List(v) => !v.is_empty(),
            CelType::Map(v) => !v.map.is_empty(),
            CelType::Int(v) => *v != 0,
            CelType::UInt(v) => *v != 0,
            CelType::Float(v) => *v != 0.0,
            CelType::String(v) => !v.is_empty(),
            CelType::Bytes(v) => !v.is_empty(),
            CelType::Bool(v) => *v,
            CelType::Null => false,
            CelType::Function(_, _) => false,
        }
    }
}

impl From<&Atom> for CelType {
    #[inline(always)]
    fn from(atom: &Atom) -> Self {
        match atom {
            Atom::Int(v) => CelType::Int(*v),
            Atom::UInt(v) => CelType::UInt(*v),
            Atom::Float(v) => CelType::Float(*v),
            Atom::String(v) => CelType::String(v.clone()),
            Atom::Bytes(v) => CelType::Bytes(v.clone()),
            Atom::Bool(v) => CelType::Bool(*v),
            Atom::Null => CelType::Null,
        }
    }
}

impl ops::Add<CelType> for CelType {
    type Output = Result<CelType, InterpreterError>;

    #[inline(always)]
    fn add(self, rhs: CelType) -> Self::Output {
        let result = match (self, rhs) {
            (CelType::Int(l), CelType::Int(r)) => CelType::Int(l + r),
            (CelType::UInt(l), CelType::UInt(r)) => CelType::UInt(l + r),

            // Float matrix
            (CelType::Float(l), CelType::Float(r)) => CelType::Float(l + r),
            (CelType::Int(l), CelType::Float(r)) => CelType::Float(l as f64 + r),
            (CelType::Float(l), CelType::Int(r)) => CelType::Float(l + r as f64),
            (CelType::UInt(l), CelType::Float(r)) => CelType::Float(l as f64 + r),
            (CelType::Float(l), CelType::UInt(r)) => CelType::Float(l + r as f64),

            (CelType::List(l), CelType::List(r)) => {
                let new = l.iter().chain(r.iter()).cloned().collect();

                CelType::List(new)
            }
            (CelType::String(l), CelType::String(r)) => {
                let mut new = String::with_capacity(l.len() + r.len());
                new.push_str(&l);
                new.push_str(&r);
                CelType::String(new.into())
            }
            _ => return Err(InterpreterError::OperandMismatch),
        };

        Ok(result)
    }
}

impl ops::Sub<CelType> for CelType {
    type Output = Result<CelType, InterpreterError>;

    #[inline(always)]
    fn sub(self, rhs: CelType) -> Self::Output {
        let result = match (self, rhs) {
            (CelType::Int(l), CelType::Int(r)) => CelType::Int(l - r),
            (CelType::UInt(l), CelType::UInt(r)) => CelType::UInt(l - r),

            // Float matrix
            (CelType::Float(l), CelType::Float(r)) => CelType::Float(l - r),
            (CelType::Int(l), CelType::Float(r)) => CelType::Float(l as f64 - r),
            (CelType::Float(l), CelType::Int(r)) => CelType::Float(l - r as f64),
            (CelType::UInt(l), CelType::Float(r)) => CelType::Float(l as f64 - r),
            (CelType::Float(l), CelType::UInt(r)) => CelType::Float(l - r as f64),

            _ => return Err(InterpreterError::OperatorNotImplemented{operator: "sub"}),
        };
        Ok(result)
    }
}

impl ops::Div<CelType> for CelType {
    type Output = Result<CelType, InterpreterError>;

    #[inline(always)]
    fn div(self, rhs: CelType) -> Self::Output {
        let result = match (self, rhs) {
            (CelType::Int(l), CelType::Int(r)) => CelType::Int(l / r),
            (CelType::UInt(l), CelType::UInt(r)) => CelType::UInt(l / r),

            // Float matrix
            (CelType::Float(l), CelType::Float(r)) => CelType::Float(l / r),
            (CelType::Int(l), CelType::Float(r)) => CelType::Float(l as f64 / r),
            (CelType::Float(l), CelType::Int(r)) => CelType::Float(l / r as f64),
            (CelType::UInt(l), CelType::Float(r)) => CelType::Float(l as f64 / r),
            (CelType::Float(l), CelType::UInt(r)) => CelType::Float(l / r as f64),

            _ => return Err(InterpreterError::OperatorNotImplemented{operator: "div"}),
        };
        Ok(result)
    }
}

impl ops::Mul<CelType> for CelType {
    type Output = Result<CelType, InterpreterError>;

    #[inline(always)]
    fn mul(self, rhs: CelType) -> Self::Output {
        let result = match (self, rhs) {
            (CelType::Int(l), CelType::Int(r)) => CelType::Int(l * r),
            (CelType::UInt(l), CelType::UInt(r)) => CelType::UInt(l * r),

            // Float matrix
            (CelType::Float(l), CelType::Float(r)) => CelType::Float(l * r),
            (CelType::Int(l), CelType::Float(r)) => CelType::Float(l as f64 * r),
            (CelType::Float(l), CelType::Int(r)) => CelType::Float(l * r as f64),
            (CelType::UInt(l), CelType::Float(r)) => CelType::Float(l as f64 * r),
            (CelType::Float(l), CelType::UInt(r)) => CelType::Float(l * r as f64),

            _ => return Err(InterpreterError::OperatorNotImplemented{operator: "mul"}),
        };
        Ok(result)
    }
}

impl ops::Rem<CelType> for CelType {
    type Output = Result<CelType, InterpreterError>;

    #[inline(always)]
    fn rem(self, rhs: CelType) -> Self::Output {
        let result = match (self, rhs) {
            (CelType::Int(l), CelType::Int(r)) => CelType::Int(l % r),
            (CelType::UInt(l), CelType::UInt(r)) => CelType::UInt(l % r),

            // Float matrix
            (CelType::Float(l), CelType::Float(r)) => CelType::Float(l % r),
            (CelType::Int(l), CelType::Float(r)) => CelType::Float(l as f64 % r),
            (CelType::Float(l), CelType::Int(r)) => CelType::Float(l % r as f64),
            (CelType::UInt(l), CelType::Float(r)) => CelType::Float(l as f64 % r),
            (CelType::Float(l), CelType::UInt(r)) => CelType::Float(l % r as f64),

            _ => return Err(InterpreterError::OperatorNotImplemented{operator: "rem"}),
        };
        Ok(result)
    }
}
