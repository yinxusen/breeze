package scalanlp.smr

import scalanlp.serialization.{ DataSerialization}
import java.io._
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import scala.collection.Parallelizable
import storage._
import java.net.URI
import actors.Futures
;

/**
 * 
 * @author dlwh
 */
abstract class ThreadDistributor extends Distributor with DistributorLike[ThreadDistributor] with Storage {
  protected def doWithShard[T:DataSerialization.Readable, A](shard: URI)(f: (Storage, T) => A) = {
    Futures.future(f(this,load[T](shard).get));
  }
}

object ThreadDistributor {

}
