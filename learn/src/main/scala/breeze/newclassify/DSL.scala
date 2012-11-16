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


  object logit
  trait linear
  object linear extends linear
  object multinomial

  /**
   * logistic loss
   * @return
   */
  implicit val logitReponse:Response[Double, logit.type, Double] = new Response[Double, logit.type, Double] {
    def lossAndGradient(yy: Double, prediction: Double): (Double, Double) = {
      val sig = sigmoid(prediction)
      val loss = yy * math.log(sig) + (1-yy) * math.log1p(sig)
      val grad = yy - sig

      (-loss) -> -grad
    }

    def sum(r: IndexedSeq[Double]): Double = r.sum
  }

  /**
   * Squared loss
   * @return
   */
  implicit val linearResponse:Response[Double, linear, Double] = new Response[Double, linear, Double] {
    def lossAndGradient(yy: Double, pred: Double): (Double, Double) = {
      val diff = yy - pred
      (0.5 * diff * diff, diff)
    }

    def sum(r: IndexedSeq[Double]): Double = r.sum
  }


  /**
   * log loss
   * @return
   */
  implicit val multinomialResponse:Response[Int, multinomial.type, DenseVector[Double]] = new Response[Int, multinomial.type, DenseVector[Double]] {
    def lossAndGradient(yy: Int, pred: DenseVector[Double]): (Double, DenseVector[Double]) = {
      val norm = softmax(pred)
      val loss = norm - pred(yy)

      val grad = exp(pred - norm)
      grad(yy) += 1.0
      (loss) -> grad
    }

    def sum(r: IndexedSeq[DenseVector[Double]]): DenseVector[Double] = {
      val res = r(0).copy
      r.drop(1) foreach {res += _ }
      res
    }
  }

  implicit val doubleLinearPredictor: Regressor[Double, Double] = new Regressor[Double, Double] {
    def preprocess(data: IndexedSeq[Double]) = new PredictorInference[Double, Double] {
      def predictor(weights: DenseVector[Double]): Predictor[Double, Double] = new SimplePredictor(weights(0))

      def weightsVector: DenseVector[Double] = {
        DenseVector.zeros[Double](1)
      }
    }
  }


  def cat[Datum, T](f: Datum=>T):Datum=>Categorical[T] = f andThen (Categorical(_))

}

trait Response[Y, -Helper, T] {
  def lossAndGradient(gold: Y, pred: T): (Double, T)
  def sum(r: IndexedSeq[T]):T
}



trait Regressor[T, R] {
  def lift[D](view: D=>T):Regressor[D, R] = new LiftedRegressor(this, view)
  def preprocess(data: IndexedSeq[T]):PredictorInference[T, R]
}

case class LiftedRegressor[T, R, U](regressor: Regressor[T, R], view: U=>T) extends Regressor[U, R] {
  def preprocess(data: IndexedSeq[U]): PredictorInference[U, R] = regressor.preprocess(data.map(view)).lift(view)
}

trait PredictorInference[T, R] {
  def predictor(weights: DenseVector[Double]): Predictor[T, R]

  def weightsVector:DenseVector[Double]
  def lift[D](view: D=>T):PredictorInference[D, R] = new LiftedPredictorInference(this, view)
}

case class LiftedPredictorInference[T, R, U](predictor: PredictorInference[T, R], view: U=>T) extends PredictorInference[U, R] {
  def predictor(weights: DenseVector[Double]): Predictor[U, R] = predictor.predictor(weights).lift(view)

  def weightsVector = predictor.weightsVector
}

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

case class LiftedPredictor[T, U, R](pred: Predictor[T, R], view: U=>T) extends Predictor[U, R] {
  def predict(ex: U): R = pred.predict(view(ex))

  def accumulateGradient(responseLoss: Double, ex: U, responseGradient: R, acc: DenseVector[Double]): DenseVector[Double] = {
    pred.accumulateGradient(responseLoss, view(ex), responseGradient, acc)
  }
}
