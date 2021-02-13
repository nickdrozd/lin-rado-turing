use std::iter;

use itertools::cloned;

use crate::types::Symbol;

#[derive(Debug, Clone)]
pub struct Tape<Symbol>(Vec<Symbol>);

impl<Sym: Symbol> Default for Tape<Sym> {
    fn default() -> Self {
        Tape((0..182).map(|_| Sym::zero()).collect::<Vec<_>>())
    }
}

impl<Sym: Symbol> Tape<Sym> {
    pub fn read(&self, pos: usize) -> Option<&Sym> {
        self.0.get(pos)
    }

    pub fn insert(&mut self) {
        self.0.insert(0, Sym::zero());
    }

    pub fn write(&mut self, pos: usize, symbol: Sym) {
        if let Some(val) = self.0.get_mut(pos) {
            *val = symbol;
        } else {
            self.0.push(symbol);
        }
    }

    pub fn iter_between<'a>(&'a self, first: usize, last: usize) -> impl Iterator<Item = Sym> + 'a {
        self.0[first..last].iter().cloned()
    }

    pub fn iter_to<'a>(&'a self, to: usize) -> impl Iterator<Item = Sym> + 'a {
        self.0[..to].iter().cloned()
    }

    pub fn iter_from<'a>(&'a self, from: usize) -> impl Iterator<Item = Sym> + 'a {
        self.0[from..].iter().cloned()
    }

    pub fn iter<'a>(&'a self) -> impl Iterator<Item = Sym> + 'a {
        self.0.iter().cloned()
    }

    pub fn size(&self) -> usize {
        self.0.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{ThreeSymbol, TwoState, TwoSymbol};

    // #[test]
    // fn test_write_iter() {
    //     let mut tape = Tape::default();

    //     tape.write(0, TwoSymbol::Zero);

    //     tape.write(-1, TwoSymbol::One);

    //     tape.write(-2, TwoSymbol::One);

    //     tape.write(1, TwoSymbol::One);

    //     assert_eq!(
    //         tape.iter_between(-2, 0).collect::<Vec<_>>(),
    //         vec![TwoSymbol::One, TwoSymbol::One]
    //     );
    // }

    // #[test]
    // fn test_write_iter2() {
    //     let mut tape = Tape::default();

    //     tape.write(0, TwoSymbol::One);

    //     tape.write(1, TwoSymbol::Zero);

    //     tape.write(2, TwoSymbol::One);

    //     assert_eq!(tape.iter().count(), 3);

    //     // assert_eq!(tape.size(), 3);
    // }

    // #[test]
    // fn test_write_size() {
    //     let mut tape = Tape::default();

    //     assert_eq!(tape.size(), 0);

    //     tape.write(0, TwoSymbol::Zero);

    //     assert_eq!(tape.size(), 1);

    //     tape.write(-1, TwoSymbol::Zero);

    //     assert_eq!(tape.size(), 2);

    //     tape.write(1, TwoSymbol::One);

    //     assert_eq!(tape.size(), 3);

    //     tape.write(2, TwoSymbol::One);

    //     assert_eq!(tape.size(), 4);
    // }
}
