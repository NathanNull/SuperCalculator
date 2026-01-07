use crate::{
    augmented_matrix::AugmentedMatrix,
    matrix::{ColumnVector, SquareMatrix},
    num::cyclic_group::ZMod,
};

pub struct LightsOutGame<const R: usize, const C: usize>
where
    [(); R * C]:,
{
    state: ColumnVector<ZMod<2>, { R * C }>,
    connections: SquareMatrix<ZMod<2>, { R * C }>,
}

impl<const R: usize, const C: usize> LightsOutGame<R, C>
where
    [(); R * C]:,
{
    pub fn new(configuration: [[bool; C]; R]) -> Self {
        let mut config_arr = [ZMod::new(0); R * C];
        let mut connect_arr = [[ZMod::new(0); R * C]; R * C];
        for (r, col) in configuration.iter().enumerate() {
            for (c, ele) in col.iter().enumerate() {
                let ind = r * C + c;
                config_arr[ind] = if *ele {
                    ZMod::new(1)
                } else {
                    ZMod::new(0)
                };
                connect_arr[ind][ind] = ZMod::new(1);
                if r > 0 {
                    connect_arr[ind][ind - C] = ZMod::new(1);
                }
                if r < R - 1 {
                    connect_arr[ind][ind + C] = ZMod::new(1);
                }
                if c > 0 {
                    connect_arr[ind][ind - 1] = ZMod::new(1);
                }
                if c < C - 1 {
                    connect_arr[ind][ind + 1] = ZMod::new(1);
                }
            }
        }
        Self {
            state: ColumnVector::v_new(config_arr),
            connections: SquareMatrix::new(connect_arr),
        }
    }

    pub fn blank() -> Self {
        Self::new([[false; C]; R])
    }

    pub fn make_move(&mut self, row: usize, col: usize) {
        let ind = row * C + col;
        let conns = self.connections.entries[ind];
        for (i, conn) in conns.iter().enumerate() {
            self.state.entries[i][0] = self.state.entries[i][0] + *conn;
        }
    }

    pub fn solve(&self) -> Vec<(usize, usize)> {
        let aug = AugmentedMatrix::new(self.connections, self.state);
        let soln = aug
            .solve()
            .expect("Matrices of this form should always be invertible")
            .right_matrix;
        let mut vec_soln = vec![];
        for r in 0..R {
            for c in 0..C {
                if soln.entries[r * C + c][0] == ZMod::new(1) {
                    vec_soln.push((r, c));
                }
            }
        }
        vec_soln
    }
}
