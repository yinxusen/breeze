package scalanlp.smr.collection

import collection.mutable.Builder
import java.net.URI

/**
 * 
 * @author dlwh
 */

trait DistributedBuilder[Elem, +To] extends Builder[Elem,To] {
  type LocalSummary
  def localBuilder:LocalBuilder[Elem,Iterable[Elem],LocalSummary];
  def resultFromSummaries(summaries: IndexedSeq[(Iterable[URI],LocalSummary)]):To;
}

trait LocalBuilder[-Elem, +To, LocalSummary] extends Builder[Elem, To] with Serializable {
  def summary():LocalSummary;
}