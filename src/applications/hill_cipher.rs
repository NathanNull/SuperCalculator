use std::array;
use std::collections::HashMap;

use itertools::Itertools;

use crate::matrix::{ColumnVector, Matrix, SquareMatrix};
use crate::num::cyclic_group::ZMod;
use crate::ring_field::Ring as _;

const ALPHABET: &str = "abcdefghijklmnopqrstuvwxyz";

fn to_nums<const BLOCK_SIZE: usize>(text: &str) -> Vec<[ZMod<26>; BLOCK_SIZE]> {
    let alphabet_map: HashMap<char, usize> =
        HashMap::from_iter(ALPHABET.chars().enumerate().map(|(i, c)| (c, i)));
    text.chars()
        .map(|c| ZMod::<26>::new(*alphabet_map.get(&c).expect("Invalid character")))
        .array_chunks::<BLOCK_SIZE>()
        .collect::<Vec<_>>()
}

pub fn try_break_code<const BLOCK_SIZE: usize>(
    ciphertext: &str,
    plaintext: &str,
) -> Option<SquareMatrix<ZMod<26>, BLOCK_SIZE>> {
    let cipher = to_nums(ciphertext);
    let plain = to_nums(plaintext);

    for vals in (0..cipher.len()).combinations(BLOCK_SIZE) {
        let cmat = Matrix::new_columns(array::from_fn(|i| cipher[vals[i]]));
        let pmat = Matrix::new_columns(array::from_fn(|i| plain[vals[i]]));
        // EM=C -> E=CM^-1
        if let Some(pinv) = pmat.try_inverse() {
            let enc_trial = cmat * pinv;
            println!(
                "{enc_trial:?}: trial encryption matrix for {vals:?}, det={:?}",
                enc_trial.determinant()
            );
            if enc_trial.determinant().try_inverse().is_some() {
                return Some(enc_trial);
            }
        }
    }

    return None;
}

pub fn decode<const BLOCK_SIZE: usize>(
    ciphertext: &str,
    encryption_matrix: SquareMatrix<ZMod<26>, BLOCK_SIZE>,
) -> Option<String> {
    if let Some(decrypt) = encryption_matrix.try_inverse() {
        let mut chars = vec![];
        for c in to_nums(ciphertext) {
            let c_col = ColumnVector::v_new(c);
            let plaintext_col = decrypt * c_col;
            for [e] in plaintext_col.entries {
                let pos = e.into();
                chars.push(&ALPHABET[pos..=pos]);
            }
        }
        return Some(chars.join(""));
    } else {
        println!(
            "Couldn't take the inverse (det={:?})",
            encryption_matrix.determinant()
        );
    }
    None
}

pub fn encode<const BLOCK_SIZE: usize>(
    ciphertext: &str,
    encryption_matrix: SquareMatrix<ZMod<26>, BLOCK_SIZE>,
) -> String {
    let mut chars = vec![];
    for c in to_nums(ciphertext) {
        let c_col = ColumnVector::v_new(c);
        let ciphertext_col = encryption_matrix * c_col;
        for [e] in ciphertext_col.entries {
            let pos = e.into();
            chars.push(&ALPHABET[pos..=pos]);
        }
    }
    chars.join("")
}
