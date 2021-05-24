use ndarray::{Array2, array}; // For matricies

#[derive(Debug, PartialEq)]
pub struct CorrectionOperatorX{
  pub matrix: Array2<i16>
}

impl CorrectionOperatorX {
  pub fn new() -> Self {
    Self {
      matrix: array![
        [0, 1],
        [1, 0],
      ],
    }
  }
}

pub struct CorrectionOperatorZ{
  pub matrix: Array2<i16>
}

impl CorrectionOperatorZ {
  pub fn new() -> Self {
    Self {
      matrix: array![
        [1, 0],
        [0, -1],
      ],
    }
  }
}
  