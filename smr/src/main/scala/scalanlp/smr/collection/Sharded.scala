package scalanlp.smr.collection

import java.net.URI

/**
 * 
 * @author dlwh
 */

trait Sharded {
  def shards: IndexedSeq[URI];
}