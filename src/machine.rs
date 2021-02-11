use crate::{
    program::Program,
    tape::Tape,
    types::{State, Symbol},
};
use itertools::Either;
use std::{collections::BTreeMap, io::Write};

type Action<S, Sym> = (S, Sym);
type Beeps<S> = BTreeMap<S, usize>;
type Snapshots<S, Sym> = BTreeMap<Action<S, Sym>, Vec<(usize, i64, i64, Tape<Sym>, Beeps<S>)>>;

pub struct Machine<State, Symbol> {
    prog: Program<State, Symbol>,
    state: State,
    pos: i64,
    tape: Tape<Symbol>,

    halt: Option<Halt>,
}

impl<S: State, Sym: Symbol> Machine<S, Sym> {
    pub const fn num(&self) -> (usize, usize) {
        self.prog.num()
    }

    pub fn new(prog: Program<S, Sym>) -> Self {
        Machine {
            prog,
            state: S::initial_state(),
            pos: 0,
            tape: Tape::default(),

            halt: None,
        }
    }

    fn read(&self) -> Option<&Sym> {
        self.tape.read(self.pos)
    }

    pub fn marks(&self) -> usize {
        self.tape.iter().filter(|item| Sym::zero() != *item).count()
    }

    fn write(&mut self, symbol: Sym) {
        self.tape.write(self.pos, symbol)
    }

    fn move_left(&mut self) {
        self.pos -= 1;
    }

    fn move_right(&mut self) {
        self.pos += 1;
    }

    pub fn halt(&mut self) -> Option<Halt> {
        self.halt
    }

    fn input_to_tape(&mut self, input: Vec<Sym>) {
        for (i, s) in input.into_iter().enumerate() {
            self.tape.write(i as i64, s);
        }
    }

    fn write_to_buffer<B: Write>(output: &mut Option<B>, step: usize, new_state: S, symbol: Sym) {
        if let Some(buffer) = output {
            writeln!(
                buffer,
                "step: {}: state={:?}, symbol: {:?}",
                step, new_state, symbol
            )
            .expect("Failed to write to stdout");
        }
    }

    fn recurr_check_init() -> (Snapshots<S, Sym>, Vec<i64>) {
        (BTreeMap::new(), vec![])
    }

    fn recurr_deviations(&self, deviations: Option<&mut Vec<i64>>, init: i64) -> i64 {
        if let Some(dev) = deviations {
            dev.push(self.pos - init);
        }
        self.pos - init
    }

    fn recurr_check(
        &self,
        step: usize,
        snapshots: Option<&mut Snapshots<S, Sym>>,
        deviations: Option<&Vec<i64>>,
        check: Option<usize>,
        init: i64,
        beeps: &Beeps<S>,
        dev: i64,
    ) -> Option<Halt> {
        if let (Some(snaps), Some(deviations), Some(recur)) = (snapshots, deviations, check) {
            if step >= recur {
                let action = (self.state, self.read().copied().unwrap_or_else(Sym::zero));

                let mut iter = if let Some(items) = snaps.get(&action) {
                    Either::Right(items.iter())
                } else {
                    Either::Left(std::iter::empty())
                };

                if let Some((pstep, step, pbeeps)) = loop {
                    if let Some((pstep, pinit, pdev, ptape, pbeeps)) = iter.next() {
                        if let Some(dev) = deviations.last().copied() {
                            let (prev, curr) = if dev < *pdev as i64 {
                                let dmax =
                                    deviations[*pstep..].iter().max().copied().unwrap_or(dev) + 1;

                                let prev = ptape.iter_to(*pinit as i64 + dmax).collect::<Vec<_>>();
                                let mut curr = self
                                    .tape
                                    .iter_to(init + dmax + dev - *pdev as i64)
                                    .collect::<Vec<_>>();

                                let mut first = vec![];
                                for i in 0..prev.len() {
                                    if curr.get(i).is_none() {
                                        first.push(Sym::zero());
                                    }
                                }

                                first.append(&mut curr);
                                (prev, first)
                            } else if (*pdev as i64) < dev {
                                let dmin =
                                    deviations[*pstep..].iter().min().copied().unwrap_or(dev);

                                let prev =
                                    ptape.iter_from(*pinit as i64 + dmin).collect::<Vec<_>>();
                                let mut curr = self
                                    .tape
                                    .iter_from(init + dmin + dev - *pdev)
                                    .collect::<Vec<_>>();
                                for i in 0..prev.len() {
                                    if curr.get(i).is_none() {
                                        curr.push(Sym::zero());
                                    }
                                }

                                (prev, curr)
                            } else {
                                let dmax =
                                    deviations[*pstep..].iter().max().copied().unwrap_or(dev) + 1;
                                let dmin =
                                    deviations[*pstep..].iter().min().copied().unwrap_or(dev);

                                let prev = ptape
                                    .iter_between(*pinit as i64 + dmin, *pinit as i64 + dmax)
                                    .collect::<Vec<_>>();
                                let curr = self
                                    .tape
                                    .iter_between(init + dmin, init + dmax)
                                    .collect::<Vec<_>>();
                                (prev, curr)
                            };
                            println!("{:?} == {:?}", prev, curr);
                            if prev == curr {
                                break Some((*pstep, step, pbeeps));
                            }
                        }
                    } else {
                        break None;
                    }
                } {
                    if pbeeps
                        .keys()
                        .all(|state| beeps.get(state) > pbeeps.get(state))
                    {
                        return Some(Halt::new(pstep, HaltReason::Recurr(step - pstep)));
                    } else {
                        return Some(Halt::new(step, HaltReason::Quasihalt(step - pstep)));
                    }
                }

                snaps
                    .entry(action)
                    .and_modify(|v| v.push((step, init, dev, self.tape.clone(), beeps.clone())))
                    .or_insert_with(|| vec![(step, init, dev, self.tape.clone(), beeps.clone())]);
            }
        }
        None
    }

    fn run_turing_step<B: Write>(&mut self, output: &mut Option<B>, step: usize, init: &mut i64) {
        let symbol = self.read().copied().unwrap_or_else(Sym::zero);
        let state = self.state;

        let &(new_state, symbol, direction) = self.prog.instruction(state, symbol);

        Self::write_to_buffer(output, step, new_state, symbol);

        self.state = new_state;

        self.write(symbol);

        match direction {
            crate::types::Direction::Left => {
                if self.pos == 0 {
                    *init += 1;
                }
                self.move_left();
            }
            crate::types::Direction::Right => self.move_right(),
        }
    }

    pub fn run_until_halt<B: Write>(
        &mut self,
        input: Vec<Sym>,
        limit: usize,
        output: &mut Option<B>,
        check_recurrence: Option<usize>,
    ) {
        let mut init = (input.len() / 2) as i64;

        self.pos = init as i64;

        let mut beeps: Beeps<S> = BTreeMap::new();

        self.input_to_tape(input);

        Self::write_to_buffer(output, 0, self.state, Sym::zero());

        let (mut snapshots, mut deviations) = if check_recurrence.is_some() {
            let f = Self::recurr_check_init();
            (Some(f.0), Some(f.1))
        } else {
            (None, None)
        };

        for step in 1..=limit {
            let dev = self.recurr_deviations(deviations.as_mut(), init);

            self.halt = self.recurr_check(
                step,
                snapshots.as_mut(),
                deviations.as_ref(),
                check_recurrence,
                init as i64,
                &beeps,
                dev,
            );

            if self.halt.is_some() {
                break;
            }

            self.run_turing_step(output, step, &mut init);

            beeps.insert(self.state, step);

            // Checks for stopping
            if self.state == S::halt() {
                self.halt = Some(Halt::new(step, HaltReason::Halt));
                break;
            }
        }

        if self.halt.is_none() {
            self.halt = Some(Halt::new(limit, HaltReason::XLimit));
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Halt {
    pub steps: usize,
    pub reason: HaltReason,
}

impl Halt {
    pub fn new(steps: usize, reason: HaltReason) -> Self {
        Halt { steps, reason }
    }

    pub fn is_halted(&self) -> bool {
        self.reason == HaltReason::Halt
    }

    pub fn is_lr_recurrence(&self) -> bool {
        match self.reason {
            HaltReason::Recurr(_) => true,
            _ => false,
        }
    }

    pub fn is_limit(&self) -> bool {
        self.reason == HaltReason::XLimit
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum HaltReason {
    Halt,
    Recurr(usize),
    XLimit,
    Quasihalt(usize),
}

pub fn run_machine<S: State, Sym: Symbol>(
    program: Program<S, Sym>,
    prog_str: &str,
    limit: usize,
    mut output: Option<Box<dyn Write>>,
    verbose: bool,
    check_recurrence: Option<usize>,
) {
    let mut machine = Machine::new(program);

    if verbose {
        machine.run_until_halt(vec![], limit, &mut output, check_recurrence);
    } else {
        machine.run_until_halt::<std::io::Stdout>(vec![], limit, &mut None, check_recurrence);
    }

    if let Some(halt) = machine.halt() {
        if let Some(w) = &mut output {
            if let Err(e) = writeln!(
                w,
                "{}: marks {} steps {} reason {:?}",
                prog_str,
                machine.marks(),
                halt.steps,
                halt.reason
            ) {
                writeln!(std::io::stderr(), "Error writing: {}", e)
                    .expect("Unable to write to stderr");
            }
        }
    }
}
