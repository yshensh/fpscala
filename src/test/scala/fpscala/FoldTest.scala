package fpscala

import org.scalatest.FunSuite

class FoldTest extends FunSuite{
  test("foldLeft") {
    /**
     * List(a, b, c).foldLeft(z)(op) equals op(op(op(z, a), b), c)
     */
  }

  test("foldRight") {
    /**
     * List(a, b, c).foldRight(z)(op) equals op(a, op(b, op(c, z)))
     */
  }
}
