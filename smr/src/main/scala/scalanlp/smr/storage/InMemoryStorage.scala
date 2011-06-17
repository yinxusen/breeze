package scalanlp.smr.storage

import scalanlp.serialization.DataSerialization.{Readable, Writable}
import java.net.URI
import java.util.concurrent.atomic.AtomicLong
import collection.mutable.{WeakHashMap}

/**
 * 
 * @author dlwh
 */

trait InMemoryStorage extends Storage {
  private val map = new WeakHashMap[URI,Any];
  protected def default(uri: URI):Option[Any] = sys.error("Not found: " + uri);
  private val anonctr = new AtomicLong(0);

  def load[T: Readable](uri: URI) = synchronized { map.get(uri).orElse(default(uri)).map(_.asInstanceOf[T])}

  def store[T: Writable](t: T) = {
    store("anon-"+anonctr.getAndIncrement, t);
  }

  def store[T: Writable](name: String, t: T) = {
    val uri = new URI("memory://"+name);
    map(uri) = t;
    uri
  }
}