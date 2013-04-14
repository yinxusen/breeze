package breeze.newclassify

import org.scalatest.FunSuite
import scala.Array
import breeze.data.Example
import breeze.linalg.{DenseVector, Counter}

/**
 * 
 * @author dlwh
 */
class ModelTest extends FunSuite {

  test("simple example") {
    val trainingData = Array (
      Example("cat",Counter.count("fuzzy","claws","small").mapValues(_.toDouble)),
      Example("bear",Counter.count("fuzzy","claws","big").mapValues(_.toDouble)),
      Example("cat",Counter.count("claws","medium").mapValues(_.toDouble))
    )
    val testData = Array(
      Example("cat", Counter.count("claws","small").mapValues(_.toDouble))
    )

    import DSL._

    import breeze.numerics.I
    val model = (
      predict[Example[String,Counter[String, Double]]](c => I(c.label == "cat"), logit)
        ~ (_.features("fuzzy"))
        + cat(_.features("claws"))
        + cat(_.features("small"))
        + cat(_.features("big"))
        + cat(_.features("medium"))
    )

    val fit = model.fit(trainingData)
    val response = fit.predict(testData.head)
    assert(response> 0.5)
    assert(response <= 1.0, response)

  }

  test("simple example dv") {
    val trainingData = Array (
      Example("cat",DenseVector(1.0, 1.0, 1.0, 0.0, 0.0)),
      Example("bear",DenseVector(1.0, 1.0, 0.0, 1.0, 0.0)),
      Example("cat",DenseVector(1.0, 0.0, 0.0, 0.0, 1.0))
    )
    val testData = Array(
      Example("cat", DenseVector(1.0, 0.0, 1.0, 0.0, 0.0))
    )

    import DSL._

    import breeze.numerics.I
    val model = (
      predict[Example[String,DenseVector[Double]]](c => I(c.label == "cat"), logit) ~ (_.features)
    )

    val fit = model.fit(trainingData)
    val response = fit.predict(testData.head)
    assert(response> 0.5)
    assert(response <= 1.0, response)

  }

  test("linear regression dv") {
    val trainingData = Array (
      Example(3.0,DenseVector(1.0, 1.0, 0.0, 0.0)),
      Example(-3.0,DenseVector(1.0, 0.0, 1.0, 0.0)),
      Example(2.0,DenseVector(0.0, 0.0, 0.0, 1.0))
    )
    val testData = Array(
      Example(3.5, DenseVector(0.0, 1.0, 0.0, 0.0))
    )

    import DSL._

    import breeze.numerics.I
    val model = (
      predict[Example[Double,DenseVector[Double]]](c => c.label, linear) ~ (_.features) + (bias)
    )

    val fit = model.fit(trainingData)
    val response = fit.predict(testData.head)
    assert(response > 3.3)
    assert(response < 3.6)

  }
}
