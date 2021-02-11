use std::collections::BTreeMap;

use crate::types::Symbol;

#[derive(Debug, Clone)]
pub struct Tape<Symbol>(BTreeMap<i64, Symbol>, i64, i64);

impl<Sym> Default for Tape<Sym> {
    fn default() -> Self {
        Tape(BTreeMap::default(), 0, 0)
    }
}

impl<Sym: Symbol> Tape<Sym> {
    pub fn min(&self) -> i64 {
        self.1
    }

    pub fn max_plus_one(&self) -> i64 {
        self.2
    }

    pub fn read(&self, pos: i64) -> Option<&Sym> {
        self.0.get(&pos)
    }

    pub fn write(&mut self, pos: i64, symbol: Sym) {
        if self.1 > pos {
            self.1 = pos;
        }

        if self.2 <= pos {
            self.2 = pos + 1;
        }

        self.0.insert(pos, symbol);
    }

    pub fn iter_between<'a>(&'a self, first: i64, last: i64) -> impl Iterator<Item = Sym> + 'a {
        (self.1.min(first)..self.2.max(last))
            .filter(move |pos| pos < &last && pos >= &first)
            .map(move |ref pos| self.0.get(pos).copied().unwrap_or_else(Sym::zero))
    }

    pub fn iter_to<'a>(&'a self, to: i64) -> impl Iterator<Item = Sym> + 'a {
        (self.1..self.2.max(to))
            .filter(move |pos| pos < &to)
            .map(move |ref pos| self.0.get(pos).copied().unwrap_or_else(Sym::zero))
    }

    pub fn iter_from<'a>(&'a self, from: i64) -> impl Iterator<Item = Sym> + 'a {
        (self.1.min(from)..self.2)
            .filter(move |pos| &from <= pos)
            .map(move |ref pos| self.0.get(pos).copied().unwrap_or_else(Sym::zero))
    }

    pub fn iter<'a>(&'a self) -> impl Iterator<Item = Sym> + 'a {
        (self.1..self.2).map(move |ref pos| self.0.get(pos).copied().unwrap_or_else(Sym::zero))
    }

    pub fn size(&self) -> usize {
        (self.2 - self.1) as usize
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{ThreeSymbol, TwoState, TwoSymbol};

    #[test]
    fn test_write_iter() {
        let mut tape = Tape::default();

        tape.write(0, TwoSymbol::Zero);

        tape.write(-1, TwoSymbol::One);

        tape.write(-2, TwoSymbol::One);

        tape.write(1, TwoSymbol::One);

        assert_eq!(
            tape.iter_between(-2, 0).collect::<Vec<_>>(),
            vec![TwoSymbol::One, TwoSymbol::One]
        );
    }

    #[test]
    fn test_write_iter2() {
        let mut tape = Tape::default();

        tape.write(0, TwoSymbol::One);

        tape.write(1, TwoSymbol::Zero);

        tape.write(2, TwoSymbol::One);

        assert_eq!(tape.iter().count(), 3);

        assert_eq!(tape.size(), 3);
    }

    #[test]
    fn test_write_size() {
        let mut tape = Tape::default();

        assert_eq!(tape.size(), 0);

        tape.write(0, TwoSymbol::Zero);

        assert_eq!(tape.size(), 1);

        tape.write(-1, TwoSymbol::Zero);

        assert_eq!(tape.size(), 2);

        tape.write(1, TwoSymbol::One);

        assert_eq!(tape.size(), 3);

        tape.write(2, TwoSymbol::One);

        assert_eq!(tape.size(), 4);
    }

    #[test]
    fn test_iter_between() {
        let mut tape = Tape::default();

        tape.write(-2, ThreeSymbol::One);

        tape.write(-1, ThreeSymbol::One);

        tape.write(0, ThreeSymbol::Two);

        tape.write(1, ThreeSymbol::Two);

        tape.write(2, ThreeSymbol::One);

        tape.write(3, ThreeSymbol::One);

        assert_eq!(
            tape.iter_between(-2, 1).collect::<Vec<_>>(),
            vec![ThreeSymbol::One, ThreeSymbol::One, ThreeSymbol::Two]
        );

        assert_eq!(
            tape.iter_between(-1, 2).collect::<Vec<_>>(),
            vec![ThreeSymbol::One, ThreeSymbol::Two, ThreeSymbol::Two]
        );

        assert_eq!(
            tape.iter_between(-5, -1).collect::<Vec<_>>(),
            vec![
                ThreeSymbol::Zero,
                ThreeSymbol::Zero,
                ThreeSymbol::Zero,
                ThreeSymbol::One
            ]
        );
    }
}
