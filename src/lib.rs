use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
};

pub mod sign_analysis;
pub trait LatticeElement: Clone + Debug + PartialEq + Eq {
    /// join operation for two lattice elements (least upper bound)
    fn join(&self, other: &Self) -> Self;

    // meet operation for two lattice elements (greatest lower bound)
    fn meet(&self, other: &Self) -> Self;

    // check if this element is less than or equal to another element (partial order)
    fn less_equal(&self, other: &Self) -> bool;

    fn is_bottom(&self) -> bool;
    fn is_top(&self) -> bool;
    fn bottom() -> Self;
    fn top() -> Self;
}

pub trait AnalysisDomain {
    type Element: LatticeElement;

    /// name of the analysis domain
    fn name() -> &'static str;
}

/// manage analytic state
pub struct Environment<E: LatticeElement> {
    /// map from variable name to lattice element
    values: HashMap<String, E>,
}

impl<E: LatticeElement> Default for Environment<E> {
    fn default() -> Self {
        Self::new()
    }
}

impl<E: LatticeElement> Environment<E> {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
        }
    }

    /// set value of a variable
    pub fn set(&mut self, var: &str, value: E) {
        self.values.insert(var.to_string(), value);
    }

    pub fn get(&self, other: &str) -> E {
        self.values.get(other).cloned().unwrap_or_else(E::top)
    }

    /// join operation for two environments
    pub fn join(&mut self, other: &Self) -> Self {
        let mut result = Environment::new();

        // collect all variables from both environments
        let mut all_vars = self.values.keys().collect::<HashSet<_>>();
        all_vars.extend(other.values.keys());

        // apply join operation for each variable
        for var in all_vars {
            let self_val = self.get(var);
            let other_val = other.get(var);
            result.set(var, self_val.join(&other_val));
        }

        result
    }

    pub fn less_equal(&self, other: &Self) -> bool {
        for (val, self_val) in &self.values {
            let other_val = other.get(val);
            if !self_val.less_equal(&other_val) {
                return false;
            }
        }

        for var in other.values.keys() {
            if !self.values.contains_key(var) {
                let self_val = self.get(var); // top value
                let other_val = other.get(var);
                if !self_val.less_equal(&other_val) {
                    return false;
                }
            }
        }

        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A simple finite-height lattice with three levels: Bottom < Middle < Top
    #[derive(Clone, Debug, PartialEq, Eq)]
    enum SimpleLattice {
        Bottom,
        Middle,
        Top,
    }

    impl LatticeElement for SimpleLattice {
        /// join operation (least upper bound)
        fn join(&self, other: &Self) -> Self {
            // Bottom join anything = that thing
            // Middle join Bottom = Middle
            // Middle join Middle = Middle
            // Middle join Top = Top
            // Top join anything = Top
            use SimpleLattice::*;
            match (self, other) {
                (Bottom, x) | (x, Bottom) => x.clone(),
                (Middle, Middle) => Middle,
                (Middle, Top) | (Top, Middle) | (Top, Top) => Top,
            }
        }

        /// meet operation (greatest lower bound)
        fn meet(&self, other: &Self) -> Self {
            // Bottom meet anything = Bottom
            // Middle meet Bottom = Bottom
            // Middle meet Middle = Middle
            // Middle meet Top = Middle
            // Top meet Top = Top
            use SimpleLattice::*;
            match (self, other) {
                (Bottom, _) | (_, Bottom) => Bottom,
                (Middle, Middle) => self.clone(),
                (Middle, Top) | (Top, Middle) => Middle,
                (Top, Top) => self.clone(),
            }
        }

        /// partial order check
        fn less_equal(&self, other: &Self) -> bool {
            // Bottom <= Middle, Bottom <= Top
            // Middle <= Top
            // Top <= Top
            // Same degree (level) treat as <=
            use SimpleLattice::*;
            match (self, other) {
                (Bottom, _) => true, // Bottom <= any
                (Middle, Bottom) => false,
                (Middle, Middle) => true,
                (Middle, Top) => true,
                (Top, Bottom) => false,
                (Top, Middle) => false,
                (Top, Top) => true,
            }
        }

        /// check if element is the bottom
        fn is_bottom(&self) -> bool {
            *self == SimpleLattice::Bottom
        }

        /// check if element is the top
        fn is_top(&self) -> bool {
            *self == SimpleLattice::Top
        }

        fn bottom() -> Self {
            SimpleLattice::Bottom
        }

        fn top() -> Self {
            SimpleLattice::Top
        }
    }

    #[test]
    fn test_simple_lattice_join() {
        // Purpose: verify that join behaves correctly for each variant
        let bottom = SimpleLattice::Bottom;
        let middle = SimpleLattice::Middle;
        let top = SimpleLattice::Top;

        assert_eq!(
            bottom.join(&bottom),
            bottom,
            "Bottom join Bottom should be Bottom"
        );
        assert_eq!(
            bottom.join(&middle),
            middle,
            "Bottom join Middle should be Middle"
        );
        assert_eq!(
            middle.join(&bottom),
            middle,
            "Middle join Bottom should be Middle"
        );
        assert_eq!(
            middle.join(&middle),
            middle,
            "Middle join Middle should be Middle"
        );
        assert_eq!(middle.join(&top), top, "Middle join Top should be Top");
        assert_eq!(top.join(&middle), top, "Top join Middle should be Top");
        assert_eq!(top.join(&top), top, "Top join Top should be Top");
    }

    #[test]
    fn test_simple_lattice_meet() {
        // Purpose: verify that meet behaves correctly for each variant
        let bottom = SimpleLattice::Bottom;
        let middle = SimpleLattice::Middle;
        let top = SimpleLattice::Top;

        assert_eq!(
            bottom.meet(&bottom),
            bottom,
            "Bottom meet Bottom should be Bottom"
        );
        assert_eq!(
            bottom.meet(&middle),
            bottom,
            "Bottom meet Middle should be Bottom"
        );
        assert_eq!(
            middle.meet(&bottom),
            bottom,
            "Middle meet Bottom should be Bottom"
        );
        assert_eq!(
            middle.meet(&middle),
            middle,
            "Middle meet Middle should be Middle"
        );
        assert_eq!(
            middle.meet(&top),
            middle,
            "Middle meet Top should be Middle"
        );
        assert_eq!(
            top.meet(&middle),
            middle,
            "Top meet Middle should be Middle"
        );
        assert_eq!(top.meet(&top), top, "Top meet Top should be Top");
    }

    #[test]
    fn test_simple_lattice_less_equal() {
        // Purpose: verify that the partial order is correct
        let bottom = SimpleLattice::Bottom;
        let middle = SimpleLattice::Middle;
        let top = SimpleLattice::Top;

        // Bottom <= everything
        assert!(bottom.less_equal(&bottom));
        assert!(bottom.less_equal(&middle));
        assert!(bottom.less_equal(&top));

        // Middle <= Top
        assert!(middle.less_equal(&middle));
        assert!(middle.less_equal(&top));
        assert!(!middle.less_equal(&bottom));

        // Top <= Top
        assert!(top.less_equal(&top));
        assert!(!top.less_equal(&bottom));
        assert!(!top.less_equal(&middle));
    }

    #[test]
    fn test_simple_lattice_is_bottom_is_top() {
        // Purpose: verify that is_bottom() and is_top() work
        let bottom = SimpleLattice::Bottom;
        let middle = SimpleLattice::Middle;
        let top = SimpleLattice::Top;

        assert!(bottom.is_bottom());
        assert!(!bottom.is_top());

        assert!(!middle.is_bottom());
        assert!(!middle.is_top());

        assert!(top.is_top());
        assert!(!top.is_bottom());
    }

    #[test]
    fn test_environment_set_and_get() {
        // Purpose: verify that Environment correctly stores and retrieves values
        let mut env = Environment::<SimpleLattice>::new();

        // by default, get should return top (in this example, SimpleLattice::Top)
        assert_eq!(env.get("x"), SimpleLattice::Top);

        env.set("x", SimpleLattice::Bottom);
        env.set("y", SimpleLattice::Middle);

        assert_eq!(
            env.get("x"),
            SimpleLattice::Bottom,
            "Expected x to be set to Bottom"
        );
        assert_eq!(
            env.get("y"),
            SimpleLattice::Middle,
            "Expected y to be set to Middle"
        );
        assert_eq!(
            env.get("z"),
            SimpleLattice::Top,
            "Uninitialized variable should default to Top"
        );
    }

    #[test]
    fn test_environment_join() {
        // Purpose: verify that environment join merges variable sets and picks the joined lattice value
        let mut env1 = Environment::<SimpleLattice>::new();
        env1.set("x", SimpleLattice::Bottom);
        env1.set("y", SimpleLattice::Middle);

        let mut env2 = Environment::<SimpleLattice>::new();
        env2.set("x", SimpleLattice::Middle);
        env2.set("z", SimpleLattice::Bottom);

        // after join:
        // x -> Bottom join Middle = Middle
        // y -> Middle join Top = Top (since env2 doesn't have y, it uses default Top)
        // z -> since env1 doesn't have z, it uses default Top
        let env_joined = env1.join(&env2);

        assert_eq!(env_joined.get("x"), SimpleLattice::Middle);
        assert_eq!(env_joined.get("y"), SimpleLattice::Top);
        assert_eq!(env_joined.get("z"), SimpleLattice::Top);
    }

    #[test]
    fn test_environment_less_equal() {
        // Purpose: verify that environment less_equal properly compares variable values
        let mut env1 = Environment::<SimpleLattice>::new();
        env1.set("x", SimpleLattice::Bottom);
        env1.set("y", SimpleLattice::Middle);

        let mut env2 = Environment::<SimpleLattice>::new();
        env2.set("x", SimpleLattice::Middle);
        // no value for y, so it defaults to Top

        // env1(x=Bottom, y=Middle) <= env2(x=Middle, y=Top)
        // because Bottom <= Middle and Middle <= Top
        assert!(env1.less_equal(&env2));

        // In the opposite direction, env2 is NOT <= env1
        // Because Middle (env2.x) is not <= Bottom (env1.x)
        // and Top (default for y in env2 if it had no 'y') is not <= Middle
        assert!(!env2.less_equal(&env1));
    }
}
