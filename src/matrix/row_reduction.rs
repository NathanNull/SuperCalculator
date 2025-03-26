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

impl<TEntry: Ring, const R: usize, const C: usize> Matrix<TEntry, R, C> {
    pub fn is_ref(&self) -> bool {
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
            for test_row in row + 1..R {
                if self.entries[test_row][*col] != TEntry::additive_ident() {
                    println!("Nonzero entry below leading entry");
                    return false;
                }
            }
        }
        // So long as the first row isn't a zero, we're good
        last_pos.is_none()
            || self.entries[0]
                .iter()
                .any(|v| *v != TEntry::additive_ident())
    }

    pub fn is_rref(&self) -> bool {
        for r in R - 1..=0 {
            let mut col = 0;
            while self.entries[r][col] == TEntry::additive_ident() {
                col += 1;
                if col >= C {
                    // This is a zero row
                    continue;
                }
            }
            // Ensure leading entry is a 1 (or equivalent)
            if self.entries[r][col] != TEntry::multiplicative_ident() {
                println!("Leading entry not a 1");
                return false;
            }

            // Ensure all higher rows have a zero in this column
            for test_row in 0..r {
                if self.entries[test_row][col] != TEntry::additive_ident() {
                    println!("Nonzero entry above leading entry");
                    return false;
                }
            }
        }
        true
    }

    pub fn pivots(&self) -> Vec<PivotPosition> {
        if !self.is_ref() {
            panic!("{self:?}: Can't find pivots of non-reduced matrix")
        }
        self.pivots_unchecked()
    }

    fn pivots_unchecked(&self) -> Vec<PivotPosition> {
        let mut pivots = vec![];
        for row in 0..R {
            for col in 0..C {
                if self.entries[row][col] != TEntry::additive_ident() {
                    pivots.push(PivotPosition::new(row, col));
                    break;
                }
            }
        }
        pivots
    }

    pub fn try_reduce_to_ref(mut self) -> Result<(Self, Vec<RowReductionStep<TEntry, R>>), ()> {
        let mut ops = vec![];
        let mut pivot_row = 0;
        let mut pivot_col = 0;
        while pivot_col < C && pivot_row < R {
            let nonzero_row_opt =
                (pivot_row..R).find(|r| self.entries[*r][pivot_col] != TEntry::additive_ident());
            if let Some(nonzero_row) = nonzero_row_opt {
                ops.push(Swap {
                    r1: nonzero_row,
                    r2: pivot_row,
                });
                self.swap(nonzero_row, pivot_row);
                for i in pivot_row + 1..R {
                    // If you can't divide, you probably can't reduce the matrix properly.
                    println!("Trying inverse of {:?}", self.entries[pivot_row][pivot_col]);
                    let mult = (self.entries[i][pivot_col]
                        * self.entries[pivot_row][pivot_col].try_inverse().ok_or(())?)
                    .negate();
                    ops.push(Add {
                        from: pivot_row,
                        to: i,
                        mult: mult,
                    });
                    self.add(pivot_row, i, mult);
                }
                pivot_col += 1;
                pivot_row += 1;
            } else {
                pivot_col += 1;
            }
        }
        Ok((self, ops))
    }

    pub fn try_reduce_to_rref(self) -> Result<(Self, Vec<RowReductionStep<TEntry, R>>), ()> {
        let (mut ref_form, mut ops) = self.try_reduce_to_ref()?;
        let leading_entries = ref_form.entries.each_ref().map(|r| {
            r.into_iter()
                .enumerate()
                .find(|(_, v)| **v != TEntry::additive_ident())
                .map(|(col, _)| col)
        });
        // Every leading entry should now be 1
        for (row, opt) in leading_entries.iter().enumerate() {
            if let Some(col) = *opt {
                let mult = ref_form.entries[row][col].try_inverse().ok_or(())?;
                ref_form.mul(row, mult);
                ops.push(Mul { row, mult });
            }
        }
        for (row, opt) in leading_entries.iter().enumerate() {
            if let Some(col) = *opt {
                for i in 0..row {
                    // No need for division here because rowXcol is a pivot, meaning it's now 1
                    let mult = ref_form.entries[i][col].negate();
                    ops.push(Add {
                        from: row,
                        to: i,
                        mult: mult,
                    });
                    ref_form.add(row, i, mult);
                }
            }
        }
        Ok((ref_form, ops))
    }

    #[allow(unused)]
    pub fn reduce_to_ref(&mut self) -> Vec<RowReductionStep<TEntry, R>>
    where
        TEntry: Field,
    {
        let (new_self, ops) = self.clone().try_reduce_to_ref().unwrap();
        self.entries = new_self.entries;
        ops
    }

    #[allow(unused)]
    pub fn reduce_to_rref(&mut self) -> Vec<RowReductionStep<TEntry, R>>
    where
        TEntry: Field,
    {
        let (new_self, ops) = self.clone().try_reduce_to_rref().unwrap();
        self.entries = new_self.entries;
        ops
    }

    fn swap(&mut self, r1: usize, r2: usize) {
        self.entries.as_mut_slice().swap(r1, r2);
    }
    fn add(&mut self, from: usize, to: usize, mult: TEntry) {
        for c in 0..C {
            self.entries[to][c] = self.entries[to][c] + (self.entries[from][c] * mult);
        }
    }
    fn mul(&mut self, row: usize, mult: TEntry) {
        for c in 0..C {
            self.entries[row][c] = self.entries[row][c] * mult;
        }
    }

    pub fn apply_ops(&mut self, ops: Vec<RowReductionStep<TEntry, R>>) {
        for op in ops {
            match op {
                Mul { row, mult } => self.mul(row, mult),
                Add { from, to, mult } => self.add(from, to, mult),
                Swap { r1, r2 } => self.swap(r1, r2),
            }
        }
    }
}

#[derive(Debug)]
pub enum RowReductionStep<TEntry: Ring, const R: usize> {
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
use RowReductionStep::*;
