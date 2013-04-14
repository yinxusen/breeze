package breeze.newclassify

import breeze.linalg._
import breeze.numerics._

/**
 *
 * @author dlwh
 */
trait Response[Y, -Helper, T] {
  def lossAndGradient(gold: Y, pred: T): (Double, T)
  def sum(r: IndexedSeq[T]):T
  def response(pred: T):T
}

object Response {
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

   def response(pred: Double): Double = sigmoid(pred)
 }

 /**
  * Squared loss
  * @return
  */
 implicit val linearResponse:Response[Double, linear, Double] = new Response[Double, linear, Double] {
   def lossAndGradient(yy: Double, pred: Double): (Double, Double) = {
     val diff = pred - yy
     (0.5 * diff * diff, diff)
   }

   def sum(r: IndexedSeq[Double]): Double = r.sum

   def response(pred: Double): Double = pred
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

   def response(pred: DenseVector[Double]): DenseVector[Double] = logNormalize(pred)
 }
}
