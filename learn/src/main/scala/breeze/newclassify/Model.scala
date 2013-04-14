package breeze.newclassify

import breeze.optimize._
import breeze.linalg.{Counter, DenseVector}
import collection.immutable


trait Model[-D, +R] extends (D=>R) {
  def predict(d: D): R

  def apply(d: D) = predict(d)
}

case class ModelSpec[Y, D, R](y: D=>Y, response: Response[Y, _, R], predictors: IndexedSeq[Regressor[D, R]]) {
  def fit(examples: IndexedSeq[D]):Model[D, R] ={
    val factories = predictors.map(_.preprocess(examples))
    val weights = factories.map(_.weightsVector)
    var i = 0
    val offsets = new Array[Int](weights.length+1)
    while(i < weights.length) {
      offsets(i+1) = offsets(i) + weights(i).length
      i += 1
    }
    val obj = new Model.Objective(this, factories, offsets, examples)
    val finalWeights = new LBFGS[DenseVector[Double]].minimize(new CachedBatchDiffFunction(obj), DenseVector.vertcat(weights:_*))
//    GradientTester.test[Int, DenseVector[Double]](obj, finalWeights, randFraction = 1.0, tolerance = 1E-9)
    val actualPredictors = obj.partitionWeights(finalWeights).zip(factories) map { case (w,f) => f.predictor(w)}

    new Model[D, R] {
      def predict(d: D): R = {
        val scores: immutable.IndexedSeq[R] = actualPredictors.map(_.predict(d))
        response.response(response.sum(scores))
      }
    }
  }

  def +[Pred](pred: D=>Pred)(implicit predictor: Regressor[Pred, R])  = new ModelSpec(y, response, predictors :+ predictor.lift(pred))
}

object Model {
  class Objective[Y, D, R](modelSpec: ModelSpec[Y, D, R],
                           factories: IndexedSeq[PredictorInference[D, R]],
                           weightOffsets: Array[Int],
                           data: IndexedSeq[D]) extends BatchDiffFunction[DenseVector[Double]] {

    /**
     * Calculates the value and gradient of the function on a subset of the data;
     */
    def calculate(weights: DenseVector[Double], batch: IndexedSeq[Int]): (Double, DenseVector[Double]) = {
      val predictors = partitionWeights(weights).zip(factories).map{case (w,f) => f.predictor(w)}
      case class Packet(grad: DenseVector[Double], var loss: Double) {
        val components = partitionWeights(grad)

        def +=(other: Packet):this.type = {
          grad += other.grad
          loss += other.loss
          this
        }
      }
      val packet = batch.par.aggregate(null:Packet) ({ (_pack, i) =>
        val d = data(i)
        val pack = if(_pack eq null) new Packet(DenseVector.zeros[Double](weights.size), 0.0) else _pack
        val scores: immutable.IndexedSeq[R] = predictors.map(_.predict(d))
        val (loss, taskGradient) = modelSpec.response.lossAndGradient(modelSpec.y(d), modelSpec.response.sum(scores))
        (0 until predictors.length).map(p => predictors(p).accumulateGradient(loss, d, taskGradient, pack.components(p)))
        pack.loss += loss
        pack
      }, { (a,b) => if (a eq null) b else if (b eq null) a else a += b})


      packet.loss -> packet.grad
    }


    def partitionWeights(weights: DenseVector[Double]) = {
      (0 until factories.length).map(i => weights.slice(weightOffsets(i), weightOffsets(i + 1)))
    }

    /**
     * The full size of the data
     */
    def fullRange: IndexedSeq[Int] = (0 until data.size)
  }


}

