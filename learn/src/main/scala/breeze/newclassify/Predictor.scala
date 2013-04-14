package breeze.newclassify

import breeze.linalg._

/**
 *
 * @author dlwh
 */
trait Predictor[T, R] {
  def predict(ex: T):R
  def accumulateGradient(responseLoss: Double, ex: T, responseGradient: R, acc: DenseVector[Double]):DenseVector[Double]

  def lift[U](view: U=>T) = LiftedPredictor(this, view)
}

case class SimplePredictor(weight: Double) extends Predictor[Double, Double] {
  def predict(ex: Double): Double = weight * ex

  def accumulateGradient(responseLoss: Double, ex: Double, responseGradient: Double, acc: DenseVector[Double]): DenseVector[Double] = {
    acc(0) += responseGradient * ex
    acc
  }
}

case class DenseVectorPredictor(weight: DenseVector[Double]) extends Predictor[DenseVector[Double], Double] {


  def accumulateGradient(responseLoss: Double, ex: DenseVector[Double], responseGradient: Double, acc: DenseVector[Double]): DenseVector[Double] = {
    axpy(responseGradient, ex, acc)
    acc
  }

  def predict(ex: DenseVector[Double]): Double = ex dot weight

}


case class LiftedPredictor[T, U, R](pred: Predictor[T, R], view: U=>T) extends Predictor[U, R] {
  def predict(ex: U): R = pred.predict(view(ex))

  def accumulateGradient(responseLoss: Double, ex: U, responseGradient: R, acc: DenseVector[Double]): DenseVector[Double] = {
    pred.accumulateGradient(responseLoss, view(ex), responseGradient, acc)
  }
}