package breeze.newclassify

import breeze.linalg._
import breeze.numerics._
import breeze.util.Index

/**
 * 
 * @author dlwh
 */
object DSL {
  def predict[Datum] = new {
    def apply[Y, ResponseMoniker, R](y: Datum=>Y, response: ResponseMoniker)(implicit _response: Response[Y, ResponseMoniker, R]) = new {
      def ~[P](p: Datum=>P)(implicit predictor: Regressor[P, R]):ModelSpec[Y, Datum, R] = new ModelSpec(y, _response, IndexedSeq(predictor.lift(p)))
    }
  }


  val multinomial : Response.multinomial.type = Response.multinomial
  val linear : Response.linear.type = Response.linear
  val logit : Response.logit.type = Response.logit

  def cat[Datum, T](f: Datum=>T):Datum=>Categorical[T] = f andThen (Categorical(_))

  trait Bias extends ((Any) => Bias)
  val bias:Bias = new Bias {def apply(x: Any) = bias}

  implicit val biasRegressor = new Regressor[Bias, Double] {
    def preprocess(data: IndexedSeq[Bias]): PredictorInference[Bias, Double] = new PredictorInference[Bias, Double] {
      def weightsVector: DenseVector[Double] = DenseVector.zeros[Double](1)

      def predictor(weights: DenseVector[Double]): Predictor[Bias, Double] = new Predictor[Bias, Double] {
        def predict(ex: Bias): Double = weights(0)

        def accumulateGradient(responseLoss: Double, ex: Bias, responseGradient: Double, acc: DenseVector[Double]): DenseVector[Double] = {
          acc(0) += responseGradient
          acc
        }
      }
    }
  }
}





