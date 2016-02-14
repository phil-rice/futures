package org.validoc.utilities

import org.scalatest.{FlatSpec, Matchers}


import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

trait FutureSpec extends FlatSpec with Matchers {

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

  def fromFuture[T](f: Future[T]) = Await.result(f, 5 seconds)

  val pageaAndChildren = (WebPage(List("ida_1", "ida_2")), List(Image("ida_1"), Image("ida_2")).reverse)
  val pagebAndChildren = (WebPage(List("idb_1", "idb_2")), List(Image("idb_1"), Image("idb_2")).reverse)
  val pagecAndChildren = (WebPage(List("idc_1", "idc_2")), List(Image("idc_1"), Image("idc_2")).reverse)
  val pagedAndChildren = (WebPage(List("idd_1", "idd_2")), List(Image("idd_1"), Image("idd_2")).reverse)
}

class LoadItemsAndChildrenGetSpec extends FutureSpec {

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
    fromFuture(loadChildren[WebPage, Image](someWebpage)) shouldBe List(Image("someId_1"), Image("someId_2")).reverse
  }

  "the LoadItemsAndChildren method loadItemAndChildren" should "load the webpage and its  children" in {
    import LoadItemsAndChildren._
    fromFuture(loadItemAndChildren[WebPage, Image]("id")) shouldBe(WebPage(List("id_1", "id_2")), List(Image("id_1"), Image("id_2")).reverse)
  }


  "the LoadItemsAndChildren method loadItemsAndChildren" should "load multiple webpages and their  children, putting results in a SuccessAndFailureList" in {
    import LoadItemsAndChildren._
    val SuccessAndFailureList(successes, failures) = fromFuture(loadItemsAndChildren[WebPage, Image](List("ida", "idb", "idc")))
    successes shouldBe List(("ida", pageaAndChildren), ("idb", pagebAndChildren), ("idc", pagecAndChildren)).reverse
    failures shouldBe List()
  }
}

class LoadItemsAndChildrenGetWithExceptionsSpec extends FutureSpec {

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
    val SuccessAndFailureList(successes, failures) = fromFuture(loadItemsAndChildren[WebPage, Image](List("ida", "idb", "idc", "idd")))
    successes shouldBe List(("ida", pageaAndChildren), ("idd", pagedAndChildren)).reverse
    failures.map(_._1) shouldBe List("idb", "idc").reverse
    failures.map(_._2.getMessage) shouldBe List("Webpage: idb", "Image: idc_1").reverse
  }

}