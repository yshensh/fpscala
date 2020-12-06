package fpscala

import fpscala.others.MyObject._
import org.scalatest.{FunSuite, Matchers}


class OthersTest extends FunSuite with Matchers {
  test("convert seq of seq to String") {
    val a = Seq(Seq("a", "b", "c"), Seq("d", "e", "f"), Seq("h", "i", "j"))
    a.flatten.mkString(".") shouldBe "a.b.c.d.e.f.h.i.j"
    a.map(_.mkString(".")) shouldBe Seq("a.b.c", "d.e.f", "h.i.j")
  }

  test ("combine a list of seq to form cartesian product") {
    val a = Seq(Seq("1", "2"), Seq("d"), Seq("h", "i"))
    combine(a) shouldBe
      Seq(Seq("1", "d", "h"), Seq("1", "d", "i")
        , Seq("2", "d", "h"), Seq("2", "d", "i"))
  }

  test ("lift seq to option[seq]") {
    seqO(Seq("a", "b", "c", "d", "e")) shouldBe Some(Seq("a", "b", "c", "d", "e"))
    seqO(Seq()) shouldBe None
  }

}
