package org.validoc.utilities


import scala.concurrent.{ExecutionContext, Future}

case class SuccessAndFailureList[T](success: List[(String, T)] = List(), failures: List[(String, Throwable)] = List()) {
  def withSuccess(id: String, t: T): SuccessAndFailureList[T] = copy(success = (id, t) :: success)

  def withFailure(id: String, t: Throwable): SuccessAndFailureList[T] = copy(failures = (id, t) :: failures)
}

object FutureHelpers {
  implicit def flattenSeq[T](f: Seq[Future[T]])(implicit ec: ExecutionContext): Future[List[T]] = Future.fold(f)(List[T]())((acc, c) => c :: acc)

  implicit def partition[T](f: Seq[(String, Future[T])])(implicit ec: ExecutionContext): Future[SuccessAndFailureList[T]] =
    f.foldLeft(Future(SuccessAndFailureList[T]())) {
      case (futureAcc, (id, futureT)) =>
        futureAcc.flatMap { acc => futureT.
          map(t => acc.withSuccess(id, t)).
          recover { case t => acc.withFailure(id, t) } }
    }
}

trait IdToItem[T] {
  def load(id: String): Future[T]
}

trait ChildIds[T] {
  def getChildIds(t: T): Seq[String]
}

object LoadItemsAndChildren {

  import FutureHelpers._

  def loadChildren[T: ChildIds, C: IdToItem](t: T)(implicit ec: ExecutionContext): Future[List[C]] =
    implicitly[ChildIds[T]].getChildIds(t).map(cId => implicitly[IdToItem[C]].load(cId))

  def loadItemsAndChildren[T: IdToItem : ChildIds, C: IdToItem](ids: Seq[String])(implicit ec: ExecutionContext): Future[SuccessAndFailureList[(T, List[C])]] =
    ids.map(id => (id, loadItemAndChildren[T, C](id)))

  def loadItemAndChildren[T: IdToItem : ChildIds, C: IdToItem](id: String)(implicit ec: ExecutionContext): Future[(T, List[C])] =
    implicitly[IdToItem[T]].load(id).flatMap { item => loadChildren[T, C](item).map(children => (item, children)) }
}



