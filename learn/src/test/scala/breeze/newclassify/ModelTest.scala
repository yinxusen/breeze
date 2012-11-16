package breeze.newclassify

import org.scalatest.FunSuite
import scala.Array
import breeze.data.Example
import breeze.linalg.Counter

/**
 * 
 * @author dlwh
 */
class ModelTest extends FunSuite {

  test("simple example") {
    val trainingData = Array (
      Example("cat",Counter.count("fuzzy","claws","small").mapValues(_.toDouble))
//      Example("bear",Counter.count("fuzzy","claws","big").mapValues(_.toDouble)),
//      Example("cat",Counter.count("claws","medium").mapValues(_.toDouble))
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
    assert(fit.predict(testData.head)> 0.5)

  }
}
