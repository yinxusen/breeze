package breeze.newclassify

import breeze.util.Index
import breeze.linalg.DenseVector

case class Categorical[T](t: T)

object Categorical {

  implicit def catPredictor[T]: Regressor[Categorical[T], Double] = new Regressor[Categorical[T], Double] {
    def preprocess(data: IndexedSeq[Categorical[T]]): PredictorInference[Categorical[T], Double] = {
      val index = Index(data.view.map(_.t))
      new PredictorInference[Categorical[T], Double] {
        def predictor(weights: DenseVector[Double]): Predictor[Categorical[T], Double] = new CatPredictor(index, weights)

        def weightsVector: DenseVector[Double] = {
          DenseVector.zeros[Double](index.size)
        }
      }
    }
  }

  case class CatPredictor[T](index: Index[T], weights: DenseVector[Double]) extends Predictor[Categorical[T], Double] {
    def predict(ex: Categorical[T]): Double = weights(index(ex.t))

    def accumulateGradient(responseLoss: Double, ex: Categorical[T], responseGradient: Double, acc: DenseVector[Double]): DenseVector[Double] = {
      acc(index(ex.t)) += responseGradient
      acc
    }
  }

}

