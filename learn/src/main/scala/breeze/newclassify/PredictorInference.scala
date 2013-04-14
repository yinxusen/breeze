package breeze.newclassify

import breeze.linalg.DenseVector

/**
 *
 * @author dlwh
 */
trait PredictorInference[T, R] {
  def predictor(weights: DenseVector[Double]): Predictor[T, R]

  def weightsVector:DenseVector[Double]
  def lift[D](view: D=>T):PredictorInference[D, R] = new LiftedPredictorInference(this, view)
}

case class LiftedPredictorInference[T, R, U](predictor: PredictorInference[T, R], view: U=>T) extends PredictorInference[U, R] {
  def predictor(weights: DenseVector[Double]): Predictor[U, R] = predictor.predictor(weights).lift(view)

  def weightsVector = predictor.weightsVector
}


