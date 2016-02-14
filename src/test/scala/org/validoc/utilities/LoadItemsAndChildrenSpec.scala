package org.validoc.utilities

import org.scalatest.{FlatSpec, Matchers}
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

trait FutureSpec extends FlatSpec with Matchers {
  def fromFuture[T](f: Future[T]) = Await.result(f, 5 seconds)
}

trait WebPageAndImageSpec extends FutureSpec {

  case class WebPage(imageUrls: List[String])

  case class Image(id: String)

  object DummyWebPageAndImageAdapters {

    import ExecutionContext.Implicits.global

    implicit object WebpageAdapter extends IdToItem[WebPage] with ChildIds[WebPage] {
      def load(id: String) = Future(WebPage(List(id + "_1", id + "_2")))

      def getChildIds(t: WebPage) = t.imageUrls
    }

    implicit object ImageAdapter extends IdToItem[Image] {
      def load(id: String): Future[Image] = Future(Image(id))
    }

  }

  val pageaAndChildren = (WebPage(List("ida_1", "ida_2")), List(Image("ida_1"), Image("ida_2")))
  val pagebAndChildren = (WebPage(List("idb_1", "idb_2")), List(Image("idb_1"), Image("idb_2")))
  val pagecAndChildren = (WebPage(List("idc_1", "idc_2")), List(Image("idc_1"), Image("idc_2")))
  val pagedAndChildren = (WebPage(List("idd_1", "idd_2")), List(Image("idd_1"), Image("idd_2")))
}

class FutureHelperSpec extends FutureSpec {

  import ExecutionContext.Implicits.global
  import FutureHelpers._

  "The FutureHelpers partition method" should "partition a Seq of Future[(String,X)] into a SuccessAndFailureList" in {
    val e3 = new RuntimeException("e3")
    val e4 = new RuntimeException("e4")
    val id1Value = ("id1", Future("value1"))
    val id2Value = ("id2", Future("value2"))
    val id3Exception = ("id3", Future.failed(e3))
    val id4Exception = ("id4", Future.failed(e4))
    val id5Value5 = ("id5", Future("value5"))

    val SuccessAndFailureSeq(successes, failures) = fromFuture(FutureHelpers.partition(
      Seq[(String, Future[String])](id1Value, id2Value, id3Exception, id4Exception, id5Value5)))
    successes shouldBe Seq(("id1", "value1"), ("id2", "value2"), ("id5", "value5"))
    failures shouldBe Seq(("id3", e3), ("id4", e4))
  }
}


class LoadItemsAndChildrenGetSpec extends WebPageAndImageSpec {

  import DummyWebPageAndImageAdapters._

  import ExecutionContext.Implicits.global

  "The Dummy Webpage loadItem TypeClass" should "load a webpage with two image ids that are based off the id" in {
    fromFuture(WebpageAdapter.load("someId")) shouldBe WebPage(List("someId_1", "someId_2"))
  }

  val someWebpage = WebPage(List("someId_1", "someId_2"))

  it should "return those childIds" in {
    import WebpageAdapter._
    getChildIds(someWebpage) shouldBe List("someId_1", "someId_2")
  }
  "The Dummy Image loadItem TypeClass" should "load an image with the given Id" in {
    fromFuture(ImageAdapter.load("someId")) shouldBe Image("someId")
  }

  "the LoadItemsAndChildren method loadChildren" should "load the children of a webpage" in {
    import LoadItemsAndChildren._
    fromFuture(loadChildren[WebPage, Image](someWebpage)) shouldBe List(Image("someId_1"), Image("someId_2"))
  }

  "the LoadItemsAndChildren method loadItemAndChildren" should "load the webpage and its  children" in {
    import LoadItemsAndChildren._
    fromFuture(loadItemAndChildren[WebPage, Image]("id")) shouldBe(WebPage(List("id_1", "id_2")), List(Image("id_1"), Image("id_2")))
  }


  "the LoadItemsAndChildren method loadItemsAndChildren" should "load multiple webpages and their  children, putting results in a SuccessAndFailureList" in {
    import LoadItemsAndChildren._
    val SuccessAndFailureSeq(successes, failures) = fromFuture(loadItemsAndChildren[WebPage, Image](List("ida", "idb", "idc")))
    successes shouldBe Seq(("ida", pageaAndChildren), ("idb", pagebAndChildren), ("idc", pagecAndChildren))
    failures shouldBe Seq()
  }
}

class LoadItemsAndChildrenGetWithExceptionsSpec extends WebPageAndImageSpec {

  import ExecutionContext.Implicits.global

  implicit object WebpageAdapter extends IdToItem[WebPage] with ChildIds[WebPage] {
    def load(id: String) = if (id == "idb") Future.failed(new RuntimeException(s"Webpage: $id")) else Future(WebPage(List(id + "_1", id + "_2")))

    def getChildIds(t: WebPage) = t.imageUrls
  }

  implicit object ImageAdapter extends IdToItem[Image] {
    def load(id: String): Future[Image] = if (id == "idc_1") Future.failed(new RuntimeException(s"Image: $id")) else Future(Image(id))
  }

  "the LoadItemsAndChildren method loadItemsAndChildren" should "separate successes and failures when exception thrown loading webpage" in {
    import LoadItemsAndChildren._
    val SuccessAndFailureSeq(successes, failures) = fromFuture(loadItemsAndChildren[WebPage, Image](List("ida", "idb", "idc", "idd")))
    successes shouldBe List(("ida", pageaAndChildren), ("idd", pagedAndChildren))
    failures.map(_._1) shouldBe List("idb", "idc")
    failures.map(_._2.getMessage) shouldBe List("Webpage: idb", "Image: idc_1")
  }
}