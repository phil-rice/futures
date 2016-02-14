package org.validoc.utilities

import scala.concurrent.{ExecutionContext, Future}

trait IdToItem[T] {
  def load(id: String): Future[T]
}

trait ChildIds[T] {
  def getChildIds(t: T): Seq[String]
}

case class SuccessAndFailureSeq[T](success: Seq[(String, T)] = Seq(), failures: Seq[(String, Throwable)] = Seq()) {
  def withSuccess(id: String, t: T): SuccessAndFailureSeq[T] = copy(success = success :+(id, t))

  def withFailure(id: String, t: Throwable): SuccessAndFailureSeq[T] = copy(failures = failures :+(id, t))
}

object FutureHelpers {
  def partition[T](f: Seq[(String, Future[T])])(implicit ec: ExecutionContext): Future[SuccessAndFailureSeq[T]] =
    f.foldLeft(Future(SuccessAndFailureSeq[T]())) {
      case (futureAcc, (id, futureT)) => futureAcc.flatMap { acc => futureT.map(t => acc.withSuccess(id, t)).recover { case t => acc.withFailure(id, t) }
      }
    }
}


object LoadItemsAndChildren {
  def loadChildren[T: ChildIds, C: IdToItem](t: T)(implicit ec: ExecutionContext): Future[Seq[C]] =
    Future.sequence(implicitly[ChildIds[T]].getChildIds(t).map(cId => implicitly[IdToItem[C]].load(cId)))


  def loadItemAndChildren[T: IdToItem : ChildIds, C: IdToItem](id: String)(implicit ec: ExecutionContext): Future[(T, Seq[C])] =
    implicitly[IdToItem[T]].load(id).flatMap { item => loadChildren[T, C](item).map(children => (item, children)) }

  def loadItemsAndChildren[T: IdToItem : ChildIds, C: IdToItem](ids: Seq[String])(implicit ec: ExecutionContext): Future[SuccessAndFailureSeq[(T, Seq[C])]] =
    FutureHelpers.partition(ids.map(id => (id, loadItemAndChildren[T, C](id))))

}



