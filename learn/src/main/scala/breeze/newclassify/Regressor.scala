package breeze.newclassify

import breeze.linalg.DenseVector


trait Regressor[T, R] {
  def lift[D](view: D=>T):Regressor[D, R] = new LiftedRegressor(this, view)
  def preprocess(data: IndexedSeq[T]):PredictorInference[T, R]
}

case class LiftedRegressor[T, R, U](regressor: Regressor[T, R], view: U=>T) extends Regressor[U, R] {
  def preprocess(data: IndexedSeq[U]): PredictorInference[U, R] = regressor.preprocess(data.map(view)).lift(view)
}



object Regressor {
  implicit val doubleLinearPredictor: Regressor[Double, Double] = new Regressor[Double, Double] {
    def preprocess(data: IndexedSeq[Double]) = new PredictorInference[Double, Double] {
      def predictor(weights: DenseVector[Double]): Predictor[Double, Double] = new SimplePredictor(weights(0))

      def weightsVector: DenseVector[Double] = {
        DenseVector.zeros[Double](1)
      }
    }
  }

  implicit val denseVectorLinearPredictor: Regressor[DenseVector[Double], Double] = new Regressor[DenseVector[Double], Double] {
    def preprocess(data: IndexedSeq[DenseVector[Double]]) = new PredictorInference[DenseVector[Double], Double] {
      val dim = data.headOption.map(_.length).getOrElse(0)
      def predictor(weights: DenseVector[Double]): Predictor[DenseVector[Double], Double] = new DenseVectorPredictor(weights)

      def weightsVector: DenseVector[Double] = {
        DenseVector.zeros[Double](dim)
      }
    }
  }
}
