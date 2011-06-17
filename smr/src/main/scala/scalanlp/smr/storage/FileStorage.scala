package scalanlp.smr.storage

import scalanlp.serialization.DataSerialization.{Writable,Readable}
import scalanlp.serialization.DataSerialization
import java.net.URI
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import java.io._
import scalanlp.smr.{CanLoadCheckpoint, CanSaveCheckpoint}
;

/**
 * 
 * @author dlwh
 */

trait FileStorage extends Storage {
  val directory: File;

  def store[T: Writable](name: String, t: T) = {
    directory.mkdirs();
    val writer = implicitly[DataSerialization.Writable[T]]
    val file = new File(directory, name)
    val oostream = new ObjectOutputStream(new GZIPOutputStream(new BufferedOutputStream(new FileOutputStream(file))));
    writer.write(oostream,t);
    oostream.close();
    file.toURI;
  }

  def load[T: Readable](uri: URI) = {
    val file = new File(uri);
    if(!file.exists) None
    else {
      val reader = implicitly[DataSerialization.Readable[T]]
      val istream = new ObjectInputStream(new GZIPInputStream(new BufferedInputStream(new FileInputStream(file))));
      val cc = reader.read(istream);
      istream.close();
      Some(cc);
    }
  }
}


