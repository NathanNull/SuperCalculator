use itertools::Itertools;

use crate::{matrix::SquareMatrix, num::real::Real};

pub fn rank<const PLAYERS: usize>(player_results: [[usize; PLAYERS]; PLAYERS]) -> [usize; PLAYERS]
where
    [(); PLAYERS + 1]:,
{
    let score_matrix: SquareMatrix<Real, PLAYERS> =
        SquareMatrix::new(player_results.map(|r| r.map(|v| Real(v as f64))));
    // We'll say that there should be exactly one eigenvalue, though that's not guaranteed
    let eigenval = score_matrix.eigenvalues().unwrap().first().unwrap().0;
    let espace = score_matrix.eigenspace(eigenval);
    let eigenvec = espace.vectors().first().unwrap();
    let order = eigenvec.as_array().map(|v| v.0);
    // Enumerate, sort by score, then convert into an array and return
    order
        .iter()
        .enumerate()
        .sorted_by_key(|(_, v)| Real(**v))
        .map(|(i, _)| i)
        .array_chunks::<PLAYERS>()
        .next()
        .unwrap()
}
