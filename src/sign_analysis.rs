use std::{
    cmp::Ordering,
    ops::{Add, Mul, Neg, Sub},
};

/// # SignLattice Partial Order Diagram
///
/// ```text
///              Top
///           /   |  \
///          /    |   \
///   NonPositive | NonNegative
///        /   \  |  /   \
///       /     \ | /     \
///   Negative   \|/   Positive
///           \   |   /
///            \  |  /
///              Zero
///               |
///             Bottom
/// ```
///
/// - **Bottom** is the least element: everything is above Bottom.
/// - **Top** is the greatest element: everything is below Top.
/// - **Zero** sits between Bottom and the "non" states (NonPositive / NonNegative).
/// - **Negative** is less than NonPositive, and **Positive** is less than NonNegative.
/// - NonPositive or NonNegative can both "expand" to Top when joined with opposing signs.
///
/// This diagram shows how each element in the sign analysis lattice relates in the partial order:
///  1. **Bottom** (no information) is below all.
///  2. **Negative**, **Zero**, **Positive** stand in the middle tier.
///  3. **NonPositive** (Negative or Zero) and **NonNegative** (Zero or Positive) are above the middle tier.
///  4. **Top** (completely unknown) is the topmost element.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SignLattice {
    /// Not yet defined state (bottom element)
    Bottom,
    /// Zero state
    Zero,
    /// Positive state
    Positive,
    /// Negative state
    Negative,
    /// Negative or zero state
    NonPositive,
    /// Positive or zero state
    NonNegative,
    /// Unknown state (top element)
    Top,
}

impl SignLattice {
    /// Convert actual value to SignLattice element
    pub fn from_value<T>(value: T) -> Self
    where
        T: PartialOrd + Default,
    {
        match value.partial_cmp(&T::default()) {
            Some(Ordering::Less) => SignLattice::Negative,
            Some(Ordering::Equal) => SignLattice::Zero,
            Some(Ordering::Greater) => SignLattice::Positive,
            None => SignLattice::Top,
        }
    }
}

impl crate::LatticeElement for SignLattice {
    fn join(&self, other: &Self) -> Self {
        use SignLattice::*;
        match (self, other) {
            // Bottom joined with any element returns that element
            (Bottom, x) | (x, Bottom) => x.clone(),

            // Join of identical elements
            (Zero, Zero) => Zero,
            (Positive, Positive) => Positive,
            (Negative, Negative) => Negative,
            (NonPositive, NonPositive) => NonPositive,
            (NonNegative, NonNegative) => NonNegative,
            (Top, _) | (_, Top) => Top,

            // Join of different elements
            (Zero, Positive) | (Positive, Zero) => NonNegative,
            (Zero, Negative) | (Negative, Zero) => NonPositive,
            (Zero, NonPositive) | (NonPositive, Zero) => NonPositive,
            (Zero, NonNegative) | (NonNegative, Zero) => NonNegative,

            (Positive, Negative) | (Negative, Positive) => Top,
            (Positive, NonPositive) | (NonPositive, Positive) => Top,
            (Positive, NonNegative) | (NonNegative, Positive) => NonNegative,

            (Negative, NonPositive) | (NonPositive, Negative) => NonPositive,
            (Negative, NonNegative) | (NonNegative, Negative) => Top,

            (NonPositive, NonNegative) | (NonNegative, NonPositive) => Top,
        }
    }

    fn meet(&self, other: &Self) -> Self {
        use SignLattice::*;
        match (self, other) {
            // Top meets with any element returns that element
            (Top, x) | (x, Top) => x.clone(),

            // Bottom always Bottom
            (Bottom, _) | (_, Bottom) => Bottom,

            // identical elements
            (Zero, Zero)
            | (Positive, Positive)
            | (Negative, Negative)
            | (NonPositive, NonPositive)
            | (NonNegative, NonNegative) => self.clone(),

            // different elements
            (Zero, Positive) | (Positive, Zero) => Bottom,
            (Zero, Negative) | (Negative, Zero) => Bottom,
            (Zero, NonPositive) | (NonPositive, Zero) => Zero,
            (Zero, NonNegative) | (NonNegative, Zero) => Zero,

            (Positive, Negative) | (Negative, Positive) => Bottom,
            (Positive, NonPositive) | (NonPositive, Positive) => Bottom,
            (Positive, NonNegative) | (NonNegative, Positive) => Positive,

            (Negative, NonPositive) | (NonPositive, Negative) => Negative,
            (Negative, NonNegative) | (NonNegative, Negative) => Bottom,

            (NonPositive, NonNegative) | (NonNegative, NonPositive) => Zero,
        }
    }

    fn less_equal(&self, other: &Self) -> bool {
        use SignLattice::*;
        match (self, other) {
            // Bottom is less than or equal to all elements
            (Bottom, _) => true,

            // No element except Top is less than or equal to Bottom
            (_, Bottom) => false,

            // All elements are less than or equal to Top
            (_, Top) => true,

            // Top is not less than or equal to any element except itself
            (Top, _) => false,

            // Comparison of identical elements
            (Zero, Zero) => true,
            (Positive, Positive) => true,
            (Negative, Negative) => true,
            (NonPositive, NonPositive) => true,
            (NonNegative, NonNegative) => true,

            // Checking hierarchy relationships
            (Zero, NonPositive) => true,
            (Zero, NonNegative) => true,
            (Positive, NonNegative) => true,
            (Negative, NonPositive) => true,

            // All other cases are not in a less_equal relationship
            _ => false,
        }
    }

    #[inline]
    fn is_bottom(&self) -> bool {
        matches!(self, SignLattice::Bottom)
    }

    #[inline]
    fn is_top(&self) -> bool {
        matches!(self, SignLattice::Top)
    }

    #[inline]
    fn bottom() -> Self {
        SignLattice::Bottom
    }

    #[inline]
    fn top() -> Self {
        SignLattice::Top
    }
}

/// sign analysis domain
pub struct SignAnalysis;

impl crate::AnalysisDomain for SignAnalysis {
    type Element = SignLattice;

    fn name() -> &'static str {
        "Sign Analysis"
    }
}

// transfer functions for arithmetic operations
impl Add for &SignLattice {
    type Output = SignLattice;

    fn add(self, rhs: Self) -> Self::Output {
        use SignLattice::*;
        match (self, rhs) {
            (Bottom, _) | (_, Bottom) => Bottom,
            (Top, _) | (_, Top) => Top,

            // 0 + x = x
            (Zero, x) => x.clone(),
            (x, Zero) => x.clone(),

            (Positive, Positive) => Positive,
            (Negative, Negative) => Negative,

            (NonPositive, NonPositive) => NonPositive,
            (NonNegative, NonNegative) => NonNegative,

            // positive + negative = unknown(Top)
            (Positive, Negative) | (Negative, Positive) => Top,

            // consider other combinations as `Top` for now.
            _ => Top,
        }
    }
}

impl Sub for &SignLattice {
    type Output = SignLattice;

    fn sub(self, rhs: Self) -> Self::Output {
        use SignLattice::*;
        match (self, rhs) {
            (Bottom, _) | (_, Bottom) => Bottom,
            (Top, _) | (_, Top) => Top,

            // x - 0 = x
            (x, Zero) => x.clone(),

            // 0 - Positive = Negative
            (Zero, Positive) => Negative,

            // 0 - Negative = Positive
            (Zero, Negative) => Positive,

            // Positive - Positive = unknown(Top)
            (Positive, Positive) => Top,

            // Negative - Negative = unknown(Top)
            (Negative, Negative) => Top,

            // Positive - Negative = Positive
            (Positive, Negative) => Positive,

            // Negative - Positive = Negative
            (Negative, Positive) => Negative,

            // Other combinations are Top
            _ => Top,
        }
    }
}

impl Mul for &SignLattice {
    type Output = SignLattice;

    fn mul(self, rhs: Self) -> Self::Output {
        use SignLattice::*;
        match (self, rhs) {
            (Bottom, _) | (_, Bottom) => Bottom,
            (Top, _) | (_, Top) => Top,

            // 0 * x = 0
            (Zero, _) | (_, Zero) => Zero,

            // positive * positive = positive
            (Positive, Positive) => Positive,

            // negative * negative = positive
            (Negative, Negative) => Positive,

            // positive * negative = negative
            (Positive, Negative) | (Negative, Positive) => Negative,

            // NonPositive * Positive = NonPositive
            (NonPositive, Positive) | (Positive, NonPositive) => NonPositive,

            // NonNegative * NonNegative = NonNegative
            (NonNegative, NonNegative) => NonNegative,

            // NonPositive * NonPositive = NonNegative
            (NonPositive, NonPositive) => NonNegative,

            // NonNegative * NonPositive = NonPositive
            (NonNegative, NonPositive) | (NonPositive, NonNegative) => NonPositive,

            // Other combinations are Top
            _ => Top,
        }
    }
}

impl Neg for &SignLattice {
    type Output = SignLattice;

    fn neg(self) -> Self::Output {
        use SignLattice::*;
        match self {
            Bottom => Bottom,
            Top => Top,
            Zero => Zero.clone(),
            Positive => Negative,
            Negative => Positive,
            NonPositive => NonNegative,
            NonNegative => NonPositive,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Environment, LatticeElement};

    #[test]
    fn test_sign_lattice_from_value() {
        assert_eq!(SignLattice::from_value(-5), SignLattice::Negative);
        assert_eq!(SignLattice::from_value(0), SignLattice::Zero);
        assert_eq!(SignLattice::from_value(10), SignLattice::Positive);
        assert_eq!(SignLattice::from_value(1.0), SignLattice::Positive);
        assert_eq!(SignLattice::from_value(-1.0), SignLattice::Negative);
        assert_eq!(SignLattice::from_value(0.0), SignLattice::Zero);
        assert_eq!(SignLattice::from_value(f32::NAN), SignLattice::Top);
        assert_eq!(SignLattice::from_value(f64::NAN), SignLattice::Top);
    }

    #[test]
    fn test_sign_lattice_join() {
        use SignLattice::*;

        assert_eq!(Bottom.join(&Zero), Zero);
        assert_eq!(Zero.join(&Positive), NonNegative);
        assert_eq!(Negative.join(&Positive), Top);
        assert_eq!(NonPositive.join(&Positive), Top);
        assert_eq!(NonNegative.join(&Negative), Top);
        assert_eq!(NonPositive.join(&NonNegative), Top);
        assert_eq!(Zero.join(&Zero), Zero);
    }

    #[test]
    fn test_sign_lattice_meet() {
        use SignLattice::*;

        assert_eq!(Top.meet(&Zero), Zero);
        assert_eq!(Zero.meet(&Positive), Bottom);
        assert_eq!(NonPositive.meet(&Zero), Zero);
        assert_eq!(NonNegative.meet(&Positive), Positive);
        assert_eq!(NonPositive.meet(&NonNegative), Zero);
        assert_eq!(Bottom.meet(&Zero), Bottom);
    }

    #[test]
    fn test_sign_lattice_less_equal() {
        use SignLattice::*;

        assert!(Bottom.less_equal(&Zero));
        assert!(Bottom.less_equal(&Top));
        assert!(Zero.less_equal(&NonPositive));
        assert!(Zero.less_equal(&NonNegative));
        assert!(Positive.less_equal(&NonNegative));
        assert!(Negative.less_equal(&NonPositive));
        assert!(Positive.less_equal(&Top));
        assert!(!Positive.less_equal(&Zero));
        assert!(!NonPositive.less_equal(&Positive));
    }

    #[test]
    fn test_sign_lattice_arithmetic() {
        use SignLattice::*;

        assert_eq!(&Positive + &Positive, Positive);
        assert_eq!(&Negative + &Negative, Negative);
        assert_eq!(&Positive + &Negative, Top);
        assert_eq!(&Zero + &Positive, Positive);

        assert_eq!(&Positive - &Negative, Positive);
        assert_eq!(&Negative - &Positive, Negative);
        assert_eq!(&Positive - &Positive, Top);

        assert_eq!(&Positive * &Positive, Positive);
        assert_eq!(&Negative * &Negative, Positive);
        assert_eq!(&Positive * &Negative, Negative);
        assert_eq!(&Zero * &Positive, Zero);

        assert_eq!(-&Positive, Negative);
        assert_eq!(-&Negative, Positive);
        assert_eq!(-&Zero, Zero);
        assert_eq!(-&NonPositive, NonNegative);
    }

    #[test]
    fn test_environment_with_sign_lattice() {
        let mut env = Environment::<SignLattice>::new();

        // In the initial state, variables are Top
        assert_eq!(env.get("x"), SignLattice::Top);

        // Setting values
        env.set("x", SignLattice::Positive);
        env.set("y", SignLattice::Negative);

        // Second environment for join operation test
        let mut env2 = Environment::<SignLattice>::new();
        env2.set("x", SignLattice::Zero);
        env2.set("z", SignLattice::NonPositive);

        // Environment join
        let joined_env = env.join(&env2);

        // x is Positive join Zero = NonNegative
        assert_eq!(joined_env.get("x"), SignLattice::NonNegative);

        // y is Negative join Top = Top (because y doesn't exist in env2)
        assert_eq!(joined_env.get("y"), SignLattice::Top);

        // z is Top join NonPositive = Top (because z doesn't exist in env)
        assert_eq!(joined_env.get("z"), SignLattice::Top);
    }

    #[test]
    fn test_sign_analysis_transfer_functions() {
        // Simple arithmetic expression analysis simulation
        let mut env = Environment::<SignLattice>::new();
        env.set("x", SignLattice::Positive);
        env.set("y", SignLattice::Negative);
        env.set("z", SignLattice::Zero);

        // x + y = Positive + Negative = Top
        let result_add = &env.get("x") + &env.get("y");
        assert_eq!(result_add, SignLattice::Top);

        // x - y = Positive - Negative = Positive
        let result_sub = &env.get("x") - &env.get("y");
        assert_eq!(result_sub, SignLattice::Positive);

        // x * y = Positive * Negative = Negative
        let result_mul = &env.get("x") * &env.get("y");
        assert_eq!(result_mul, SignLattice::Negative);

        // z * y = Zero * Negative = Zero
        let result_zero_mul = &env.get("z") * &env.get("y");
        assert_eq!(result_zero_mul, SignLattice::Zero);

        // -y = -Negative = Positive
        let result_neg = -&env.get("y");
        assert_eq!(result_neg, SignLattice::Positive);
    }
}
