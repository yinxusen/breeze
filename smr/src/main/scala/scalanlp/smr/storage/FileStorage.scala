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
  private var anonCount = 0;
  private def nextId = synchronized {
    anonCount += 1
    anonCount
  }


  def store[T: Writable](name: String, t: T): URI = {
    val writer = implicitly[DataSerialization.Writable[T]]
    val file = new File(directory, name)
    file.mkdirs()
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

  def store[T: Writable](t: T): URI = {
    store("_anon/piece-"+nextId, t)
  }

  def name(shards: IndexedSeq[URI], name: String) = {
    new File(directory,name).mkdirs()
    for( (shard,i) <- shards.toArray.zipWithIndex) yield {
      val newFile = new File(directory,name + "/"+name+"-"+i)
      Runtime.getRuntime.exec(Array("ln",new File(shard).toString,newFile.toString))
      newFile.toURI
    }
  }

  def shardsFor(name: String) = {
    val file = new File(directory, name)
    if(!file.exists) None
    else Some(new File(directory,name).listFiles().map(_.toURI))
  }
}


