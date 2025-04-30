use crate::expression::polynomial::Polynomial;
use crate::matrix;
use crate::matrix::{ColumnVector, Matrix, SquareMatrix};
use crate::num::{cyclic_group::ZMod, rational::Rational};
use crate::ring_field::Ring;
use crate::vector_space::Vector;
use crate::{
    applications::{
        geometry::polygon_area,
        hill_cipher::{decode, encode, try_break_code},
        lights_out::LightsOutGame,
        linear_regression::line_of_best_fit,
        markov_chain::SimpleMarkovChain,
        polynomial_calculus::Calculus,
        ranking::rank,
    },
    vector_space::subspace::Subspace,
};
use crate::{data, r, zmatrix};
use rand::rng;

#[allow(unused)]
pub enum Examples {
    HillCipher,
    MarkovChain,
    PolynomialCalculus,
    LightsOut,
    LinearRegression,
    Geometry,
    Eigen,
    Ranking,
    Subspace,
    RankNullity,
}

impl Examples {
    pub fn run(&self) {
        match self {
            Self::HillCipher => {
                println!(
                    "{:?}: discovered encryption matrix",
                    try_break_code::<3>(
                        &"ZSALFRLKOVDFWAUBCO".to_lowercase(),
                        &"CRYPTOGRAPHYISCOOL".to_lowercase(),
                    )
                );
                println!(
                    "Encoded string: {:?}",
                    encode(&"bloodborneislife".to_lowercase(), zmatrix!(<26>6,3;9,8))
                );
                println!(
                    "Decoded string: {:?}",
                    decode(&"NTWEVJFCMTYIMHQZ".to_lowercase(), zmatrix!(<26>6,3;9,8))
                );
            }
            Self::MarkovChain => {
                let chain = SimpleMarkovChain::new([
                    [r!(6 / 10), r!(3 / 10), r!(1 / 10)],
                    [r!(2 / 10), r!(3 / 10), r!(5 / 10)],
                    [r!(4 / 10), r!(1 / 10), r!(5 / 10)],
                ]);
                println!(
                    "{:?}: State after state 1",
                    chain.step_probabilities(ColumnVector::v_new([r!(1), r!(0), r!(0)]))
                );
                println!(
                    "{} is the steady state",
                    chain
                        .steady_state()
                        .map(|m| format!("{m:?}"))
                        .unwrap_or("None".to_string())
                );
            }
            Self::PolynomialCalculus => {
                // 3x^2+x
                let f: Polynomial<Rational, 3> = Polynomial::new(vec![(r!(3), 2), (r!(1), 1)]);
                println!(
                    "{:?}",
                    Polynomial::from_column(&(Calculus::DERIVATIVE * f.to_column()))
                );
                println!(
                    "{:?}",
                    Polynomial::from_column(&(Calculus::INTEGRAL * f.to_column()))
                );
            }
            Self::LightsOut => {
                let mut game = LightsOutGame::<5, 5>::blank();
                game.make_move(1, 1);
                game.make_move(2, 2);
                game.make_move(3, 3);
                println!("{:?}", game.solve());
            }
            Self::LinearRegression => {
                let data = data!((-2, -10), (-1, 2), (3, 8));
                println!(
                    "Line of best fit for {data:?} is {:?}",
                    line_of_best_fit(data)
                );
            }
            Self::Eigen => {
                let m = matrix!(-5,4,2;-10,5,-2;8,-4,1);
                println!("{m:?} is m");
                if let Some((p, d)) = m.try_diagonalize() {
                    println!("{:?} is P", p);
                    println!("{:?} is D", d);
                    println!("{:?} is P^-1", p.try_inverse().unwrap());
                    println!("{:?} should equal m", p * d * p.try_inverse().unwrap())
                }
                println!("{:?}: eigenspace from value 3", m.eigenspace(r!(3)));
                println!(
                    "{:?}: generalized eigenspace of rank 2 from value 3",
                    m.generalized_eigenspace(r!(3), 3)
                );
            }
            Self::Geometry => {
                // Must list points in ccw order, otherwise the area will be negative
                let polygon = data!((1, 1), (-1, 1), (-1, -1), (1, -1))
                    .into_iter()
                    .map(|p| ColumnVector::v_new([p.0, p.1]))
                    .collect::<Vec<_>>();
                println!("Polygon's area is {:?}", polygon_area(polygon));
            }
            Self::Ranking => {
                let game_data = [[0, 33, 5], [10, 0, 20], [30, 20, 0]];
                println!("{:?}: Ranking for data {game_data:?}", rank(game_data));
            }
            Self::Subspace => {
                let m = matrix!(1,0,0;0,1,0;0,0,0);
                let s = m.column_space();
                println!("Col(m) contains (1,0,1): {}", s.contains(matrix!(1;0;1)));
                let s2 = Subspace::new(
                    data!((1, 1), (1, 0), (0, 1)).map(|p| ColumnVector::v_new([p.0, p.1])),
                );
                println!(
                    "Subspace is linearly independant? {}",
                    s2.linearly_independant()
                );
                println!("Subspace's dimension: {}", s2.dimension());
                println!("{:?}: Example vector in subspace", s2.sample(false));
            }
            Self::RankNullity => {
                const M_SIZE: usize = 7;
                let m = SquareMatrix::<Rational, M_SIZE>::generate(&mut rng(), true);
                println!("{m:?}: Test matrix, randomly generated");
                println!("Rank: {}, Nullity: {}", m.rank(), m.nullity());
                assert_eq!(m.rank() + m.nullity(), M_SIZE);
            }
        }
    }
}
