use super::*;

#[derive(Debug)]
pub struct PivotPosition {
    pub row: usize,
    pub col: usize,
}

impl PivotPosition {
    pub fn new(row: usize, col: usize) -> Self {
        Self { row, col }
    }
}

pub trait RowReduction<TEntry: Ring>: Matrix<TEntry> {
    fn is_ref(&self) -> bool {
        let mut last_pos: Option<(usize, usize)> = None;
        for PivotPosition { row, col } in self.pivots_unchecked().iter().rev() {
            // Ensure leading entry is to the right of the last
            if last_pos.is_some_and(|p| *col >= p.1) {
                println!("Leading entry to left of previous ({col}, {last_pos:?})");
                return false;
            } else if last_pos.is_some_and(|p| p.0 != row + 1) {
                println!("Zero row in between nonzero rows ({row}, {last_pos:?})");
                return false;
            }
            last_pos = Some((*row, *col));

            // Ensure all lower rows have a zero in this column
            for test_row in row + 1..self.size().0 {
                if self.get_entry(test_row, *col) != Some(&TEntry::zero()) {
                    println!("Nonzero entry below leading entry");
                    return false;
                }
            }
        }
        // So long as the first row isn't a zero, we're good
        last_pos.is_none() || self.row(0).any(|v| *v != TEntry::zero())
    }

    fn is_rref(&self) -> bool {
        let size = self.size();
        for r in size.0 - 1..=0 {
            let mut col = 0;
            while self.get_entry(r, col) == Some(&TEntry::zero()) {
                col += 1;
                if col >= size.1 {
                    // This is a zero row
                    continue;
                }
            }
            // Ensure leading entry is a 1 (or equivalent)
            if self.get_entry(r, col) != Some(&TEntry::one()) {
                println!("Leading entry not a 1");
                return false;
            }

            // Ensure all higher rows have a zero in this column
            for test_row in 0..r {
                if self.get_entry(test_row, col) != Some(&TEntry::zero()) {
                    println!("Nonzero entry above leading entry");
                    return false;
                }
            }
        }
        true
    }

    fn pivots(&self) -> Vec<PivotPosition> {
        if !self.is_ref() {
            panic!("{self:?}: Can't find pivots of non-reduced matrix")
        }
        self.pivots_unchecked()
    }

    fn pivots_unchecked(&self) -> Vec<PivotPosition> {
        let mut pivots = vec![];
        let size = self.size();
        for row in 0..size.0 {
            for col in 0..size.1 {
                if self.get_entry(row, col) != Some(&TEntry::zero()) {
                    pivots.push(PivotPosition::new(row, col));
                    break;
                }
            }
        }
        pivots
    }

    fn try_reduce_to_ref(mut self) -> Result<(Self, Vec<RowReductionStep<TEntry>>), ()> {
        let mut ops = vec![];
        let mut pivot_row = 0;
        let mut pivot_col = 0;
        let size = self.size();
        while pivot_col < size.1 && pivot_row < size.0 {
            let nonzero_row_opt = (pivot_row..size.0)
                .find(|r| self.get_entry(*r, pivot_col) != Some(&TEntry::zero()));
            if let Some(nonzero_row) = nonzero_row_opt {
                ops.push(RRS::Swap {
                    r1: nonzero_row,
                    r2: pivot_row,
                });
                self.swap(nonzero_row, pivot_row);
                for i in pivot_row + 1..size.0 {
                    // If you can't divide, you probably can't reduce the matrix properly.
                    //println!("Trying inverse of {:?}", self.entries[pivot_row][pivot_col]);
                    let mult = (self
                        .get_entry(i, pivot_col)
                        .expect("should be in bounds")
                        .clone()
                        * self
                            .get_entry(pivot_row, pivot_col)
                            .expect("should be in bounds")
                            .try_inverse()
                            .ok_or(())?)
                    .negate();
                    ops.push(RRS::Add {
                        from: pivot_row,
                        to: i,
                        mult: mult.clone(),
                    });
                    RowReduction::add(&mut self, pivot_row, i, mult);
                }
                pivot_col += 1;
                pivot_row += 1;
            } else {
                pivot_col += 1;
            }
        }
        Ok((self, ops))
    }

    fn try_reduce_to_rref(self) -> Result<(Self, Vec<RowReductionStep<TEntry>>), ()> {
        let (mut ref_form, mut ops) = self.try_reduce_to_ref()?;
        let leading_entries = ref_form
            .as_ref()
            .entries
            .iter()
            .map(|r| {
                r.iter()
                    .enumerate()
                    .find(|(_, v)| ***v != TEntry::zero())
                    .map(|(col, _)| col)
            })
            .collect_vec();
        // Every leading entry should now be 1
        for (row, opt) in leading_entries.iter().enumerate() {
            if let Some(col) = *opt {
                let mult = ref_form
                    .get_entry(row, col)
                    .expect("should be valid")
                    .clone()
                    .try_inverse()
                    .ok_or(())?;
                RowReduction::mul(&mut ref_form, row, mult.clone());
                ops.push(RRS::Mul { row, mult });
            }
        }
        for (row, opt) in leading_entries.iter().enumerate() {
            if let Some(col) = *opt {
                for i in 0..row {
                    // No need for division here because rowXcol is a pivot, meaning it's now 1
                    let mult = ref_form
                        .get_entry(i, col)
                        .expect("should be valid")
                        .negate();
                    ops.push(RRS::Add {
                        from: row,
                        to: i,
                        mult: mult.clone(),
                    });
                    RowReduction::add(&mut ref_form, row, i, mult);
                }
            }
        }
        Ok((ref_form, ops))
    }

    #[allow(unused)]
    fn reduce_to_ref(&mut self) -> Vec<RowReductionStep<TEntry>>
    where
        TEntry: Field,
    {
        let (new_self, ops) = self.clone().try_reduce_to_ref().unwrap();
        *self = new_self;
        ops
    }

    #[allow(unused)]
    fn reduce_to_rref(&mut self) -> Vec<RowReductionStep<TEntry>>
    where
        TEntry: Field,
    {
        let (new_self, ops) = self.clone().try_reduce_to_rref().unwrap();
        *self = new_self;
        ops
    }

    fn swap(&mut self, r1: usize, r2: usize) {
        let size = self.size();
        for col in 0..size.1 {
            let e1 = self.get_entry(r1, col).expect("Should be valid").clone();
            let e2 = self.get_entry(r2, col).expect("Should be valid").clone();
            self.set_entry(r1, col, e2);
            self.set_entry(r2, col, e1);
        }
    }
    fn add(&mut self, from: usize, to: usize, mult: TEntry) {
        let size = self.size();
        for col in 0..size.1 {
            let e1 = self.get_entry(from, col).expect("Should be valid").clone();
            let e2 = self.get_entry(to, col).expect("Should be valid").clone();
            self.set_entry(to, col, (e1 * mult.clone()) + e2);
        }
    }
    fn mul(&mut self, row: usize, mult: TEntry) {
        let size = self.size();
        for col in 0..size.1 {
            let e1 = self.get_entry(row, col).expect("Should be valid").clone();
            self.set_entry(row, col, e1 * mult.clone());
        }
    }

    fn apply_ops(&mut self, ops: Vec<RowReductionStep<TEntry>>) {
        for op in ops {
            match op {
                RRS::Mul { row, mult } => self.mul(row, mult),
                RRS::Add { from, to, mult } => self.add(from, to, mult),
                RRS::Swap { r1, r2 } => self.swap(r1, r2),
            }
        }
    }
}

impl<TEntry: Ring, M: Matrix<TEntry>> RowReduction<TEntry> for M {}

#[derive(Debug)]
pub enum RowReductionStep<TEntry: Ring> {
    Mul {
        row: usize,
        mult: TEntry,
    },
    Add {
        from: usize,
        to: usize,
        mult: TEntry,
    },
    Swap {
        r1: usize,
        r2: usize,
    },
}
use RowReductionStep as RRS;
