package scalanlp.smr
package example

import scalanlp.util._;

import scalala.tensor.dense.DenseVector
import scalanlp.serialization.DataSerialization

/**
 * 
 * @author dlwh
 */
object LogisticRegressionExample {
  def main(args: Array[String]) {
    val distributor = new ThreadDistributor with scalanlp.smr.storage.InMemoryStorage;
    import distributor._;
    val data: IndexedSeq[DenseVector[Double]] = IndexedSeq.fill(1000)(DenseVector.rand(1000));

    val ddata = distribute(data);
    ddata map(_ + 1.0) foreach println;

  }
}