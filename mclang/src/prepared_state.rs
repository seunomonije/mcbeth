use ndarray::{Array2, array}; // For matricies
use std::f64::consts::FRAC_1_SQRT_2; // for 1/sqrt(2)

#[derive(Debug, PartialEq)]
pub struct PreparedState {
  pub state: Array2<f64>,
}

impl PreparedState {
  pub fn new() -> Self {
    Self {
      state: array![
        [FRAC_1_SQRT_2],
        [FRAC_1_SQRT_2]
      ],
    }
  }
}
  