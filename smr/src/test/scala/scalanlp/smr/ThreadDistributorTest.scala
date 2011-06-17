package scalanlp.smr

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import storage.InMemoryStorage

/**
 * 
 * @author dlwh
 */

@RunWith(classOf[JUnitRunner])
class ThreadDistributorTest extends FunSuite {

  test("Basics") {
    val dist = new ThreadDistributor with InMemoryStorage;
    val x = 0 until 100;
    dist.distribute(x).map(_ * 2).sum;
  }


}