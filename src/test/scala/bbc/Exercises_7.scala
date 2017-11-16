package bbc

import java.util.concurrent.{Executors, Future}

import org.scalatest.{FunSpec, Matchers}
import bbc.Par._

class Exercises_7 extends FunSpec with Matchers {

  describe("Par") {

    val executorService = Executors.newFixedThreadPool(10);

    it("should do something simple") {
      var unitPar = unit(5)
      val result = unit(5)(executorService)

      result.get() shouldBe 5
      result.isDone shouldBe true
    }

    it("should be able to fork to run a calculation(?)") {
      var unitPar = unit("blah")
      val result = fork(unitPar)(executorService)

      result.get() shouldBe "blah"
      result.isDone shouldBe true

    }

    it("shoudl lift a function to a parallel function") {
      def add(a: Int, b: Int): Int = a + b
      val (a,b) = (unit(24),unit(23))

      val result = map2(a,b)(add)(executorService)

      result.get() shouldBe 47
      result.isDone shouldBe true
    }

    describe("sequence") {
      it("should propagate empty") {
        sequence(List.empty[Par[Int]])(executorService) shouldBe unit(List.empty[Int])(executorService)
      }

      it("should lift the Parallel to the outside") {
        val seqResult = sequence(List(unit(1),unit(2)))(executorService).get()
        val expected = unit(List(1,2))(executorService).get()
        seqResult shouldBe expected
      }

    }

    describe("parFilter") {
      val items = List(1,2,3,4,5,6)

      it("should propagate empty") {
        parFilter(List.empty[String])(x => x.isEmpty())(executorService).get() shouldBe List.empty[String]
      }

      it("should include items matching the predicate") {
        parFilter(items)(x => x % 2 == 0)(executorService).get() shouldBe List(2, 4, 6)
      }

      ignore("should exclude items not matching the predicate") {}

    }

  }

}
