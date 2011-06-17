package scalanlp.smr

import scalanlp.distributed.{SocketDispatch, SocketService}

/**
 * 
 * @author dlwh
 */

abstract class SocketDistributor(socketDispatch: SocketDispatch=SocketDispatch()) extends Distributor {
  def this(port: Int) = this(SocketDispatch(port));

}