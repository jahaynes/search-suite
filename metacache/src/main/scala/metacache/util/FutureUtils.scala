package metacache.util

import java.util.concurrent.CompletionStage

import com.google.common.util.concurrent.{FutureCallback, Futures, ListenableFuture}
import com.twitter.util.{Future, Promise}

object FutureUtils {

  val global = scala.concurrent.ExecutionContext.global

  def convert[T](lf: ListenableFuture[T]): Future[T] = {
    val promise = Promise[T]()
    val callback = new FutureCallback[T] {
      override def onSuccess(result: T) = promise setValue result
      override def onFailure(t: Throwable) = promise setException t
    }
    Futures.addCallback(lf, callback, global)
    promise
  }

  def convert[T](cs: CompletionStage[T]): Future[T] = {
    val promise = Promise[T]()
    cs.handleAsync(
      (x: T, t: Throwable) => {
        if (t == null) {promise.setValue(x)} else {promise.setException(t)}
        x
      },
      global
    )
    promise
  }
}
