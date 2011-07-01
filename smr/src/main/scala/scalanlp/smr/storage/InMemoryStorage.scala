package scalanlp.smr.storage

import scalanlp.serialization.DataSerialization.{Readable, Writable}
import java.net.URI
import java.util.concurrent.atomic.AtomicLong
import collection.mutable.{ArrayBuffer, HashMap, WeakHashMap}

/**
 * 
 * @author dlwh
 */

trait InMemoryStorage extends Storage {
  private val map = new WeakHashMap[URI,Any]
  private val persistent = new HashMap[String,IndexedSeq[Any]]
  protected def default(uri: URI):Option[Any] = sys.error("Not found: " + uri)
  private val anonctr = new AtomicLong(0)

  private def grabPersistent[T](uri: URI): Option[T] = {
    val path = uri.getPath
    val parts = path.split("/")
    persistent.get(parts(0)).map(_ apply parts(1).toInt).orElse(default(uri)).map(_.asInstanceOf[T])
  }

  def load[T: Readable](uri: URI) = synchronized {
    if(uri.getScheme == "tmp")
      map.get(uri).orElse(default(uri)).map(_.asInstanceOf[T])
    else {
      grabPersistent(uri)
    }
  }

  def store[T: Writable](t: T) = {
    val uri = new URI("tmp://"+anonctr.getAndIncrement)
    map(uri) = t
    uri
  }

  def store[T: Writable](name: String, t: T) = {
    val uri = new URI("memory://"+name+"/0")
    persistent(name) = IndexedSeq(t)
    uri
  }

  def name(shards: IndexedSeq[URI], name: String):IndexedSeq[URI] = {
    val pieces = for( s <- shards) yield {
      if(s.getScheme == "tmp") map(s)
      else grabPersistent(s).get
    }
    persistent(name) = shards
    (0 until pieces.length) map (i => new URI("memory://"+name+"/"+i))
  }

  def shardsFor(name: String) = for {
    pieces <- persistent.get(name)
  } yield  (0 until pieces.length) map (i => new URI("memory://"+name+"/"+i))
}