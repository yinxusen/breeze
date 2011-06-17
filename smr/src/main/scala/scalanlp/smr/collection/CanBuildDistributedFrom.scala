package scalanlp.smr.collection

import collection.generic.CanBuildFrom
import java.net.URI
import scalanlp.serialization.DataSerialization

/**
 * 
 * @author dlwh
 */

trait CanBuildDistributedFrom[-This,B,That] extends CanBuildFrom[This,B,That] {
  def apply(t: This):DistributedBuilder[B,That]
  def apply():DistributedBuilder[B,That]
  def serializeB: DataSerialization.ReadWritable[B];

}