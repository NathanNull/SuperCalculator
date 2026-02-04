use crate::{
    matrix::{Column, ColumnVector, SizedMatrix},
    num::rational::Rational,
    r,
};

/// Needs to be counterclockwise I believe, quadrant I->II->III->IV <br/>
/// eg. vecs = [matrix!(5;9), matrix!(-2;3), matrix!(2;-8), matrix!(9;-2)];
pub fn polygon_area(vecs: Vec<ColumnVector<Rational, 2>>) -> Rational {
    let mut area = r!(0);
    for i in 0..vecs.len() {
        let j = (i + 1) % vecs.len();
        let m = SizedMatrix::<_, 2, 2>::new_columns(
            [&vecs[i], &vecs[j]].map(|v| *v.as_vec().as_array().unwrap()),
        );
        area = area + m.determinant() / r!(2);
    }
    area
}
