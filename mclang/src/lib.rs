/*
  File dependencies
*/
use ndarray::{Array2, array}; // For matricies
use std::f64::consts::FRAC_1_SQRT_2; // for 1/sqrt(2)

/*
  Custom imports
*/
mod prepared_state;
mod correction_operators;

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn parse_prepare_state() {
    // Redundant, as I learn rust more I'll clean up
    let plus_state = array![
      [FRAC_1_SQRT_2],
      [FRAC_1_SQRT_2]
    ];

    let prepared_state = prepared_state::PreparedState::new();
    assert_eq!(prepared_state.state, plus_state);
  }

  #[test]
  fn parse_correction_x() {
    let pauli_x = array![
      [0, 1],
      [1, 0],
    ];

    let correction_operator = correction_operators::CorrectionOperatorX::new();
    assert_eq!(correction_operator.matrix, pauli_x)
  }

  #[test]
  fn parse_correction_z() {
    let pauli_z = array![
      [1, 0],
      [0, -1],
    ];

    let correction_operator = correction_operators::CorrectionOperatorZ::new();
    assert_eq!(correction_operator.matrix, pauli_z)
  }
}


#[derive(Debug, PartialEq)]
/*
  "Tokens", right now we're attributing these string
  values in our prospective PL to the following operations.
  Discussion, needs implementation.
*/
pub enum Cmd {
  N,
  E,
  M,
  X,
  Z,
}

