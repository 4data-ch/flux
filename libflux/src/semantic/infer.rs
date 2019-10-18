use crate::semantic::types::{Kind, MonoType};
use std::ops;

#[derive(Debug, PartialEq)]
enum Constraint {
    Kind(MonoType, Kind),
    Equal(MonoType, MonoType),
}

#[derive(Debug, PartialEq)]
struct Constraints(Vec<Constraint>);

impl ops::Add for Constraints {
    type Output = Constraints;

    fn add(self, cons: Constraints) -> Self::Output {
        let mut lv = self.0;
        let mut rv = cons.0;
        (&mut lv).append(&mut rv);
        Constraints(lv)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::types::Tvar;

    #[test]
    fn add_constraints() {
        let c0 = Constraints(vec![
            Constraint::Equal(MonoType::Var(Tvar(0)), MonoType::Var(Tvar(1))),
            Constraint::Kind(MonoType::Var(Tvar(1)), Kind::Addable),
        ]);
        let c1 = Constraints(vec![
            Constraint::Equal(MonoType::Var(Tvar(2)), MonoType::Var(Tvar(3))),
            Constraint::Kind(MonoType::Var(Tvar(3)), Kind::Divisible),
        ]);
        assert_eq!(
            c0 + c1,
            Constraints(vec![
                Constraint::Equal(MonoType::Var(Tvar(0)), MonoType::Var(Tvar(1))),
                Constraint::Kind(MonoType::Var(Tvar(1)), Kind::Addable),
                Constraint::Equal(MonoType::Var(Tvar(2)), MonoType::Var(Tvar(3))),
                Constraint::Kind(MonoType::Var(Tvar(3)), Kind::Divisible),
            ])
        );
    }
}
