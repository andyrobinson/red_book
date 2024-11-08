package bbc

import java.util.concurrent.{Executors, Future}

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import bbc.Par._

class Exercises_7 extends AnyFunSpec with Matchers {

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

    describe("map theorems") {

      trait Monad[A] {
        def unit[A](a:A): Monad[A]

      }

      def id[A](a: A): A = a

      def map[A,B,M[_]](a: M[A])(f: A => B): M[B] = ???

      //     map(map(y)(g))(f)
      //
      // substitute y = unit(x)
      //
      // === map(map(unit(x))(g))(f)
      // === map(unit(g(x)))(f)    -  theorem map(unit(x))(f) == unit(f(x))
      // === unit(f(g(x))          -  ditto
      // === unit((f compose g)(x)) - definition of compose
      // === map(unit(x)) (f compose g)
      // === map(y)(f compose g)    - reverse substitution
    }

    describe("7.11 choiceN") {
      it("choose an execution based on the value returned from the first function") {
        val items = List(unit("A"),unit("B"),unit("C"))
        val result = choiceN(unit(1))(items)(executorService).get()
        result shouldBe "B"
      }

      it("should implement boolean choice based on choiceN") {
        choice(unit(true))(unit("yes"),unit("no"))(executorService).get shouldBe "yes"
        choice(unit(false))(unit("yes"),unit("no"))(executorService).get shouldBe "no"
      }
    }

    // we already implemented flatMap for the previous exercise, and have therefore
    // already completed choice and choiceN in terms of flatMap
    describe("7.13 flatMap") {
      it("applies the function to the result of the first parallel execution") {
        val start = unit(5)
        def mapFn(x: Int) = unit(x*2)

        flatMap(start)(mapFn)(executorService).get shouldBe 10
      }

      it("should implement boolean choice based on flatMap") {
        choiceFlatMap(unit(true))(unit("yes"),unit("no"))(executorService).get shouldBe "yes"
        choiceFlatMap(unit(false))(unit("yes"),unit("no"))(executorService).get shouldBe "no"
      }
    }

    describe("7.14 join (flatten?)") {
      it("executes a parallel operation within an operation") {
        def nested = unit(unit("A"))
        join(nested)(executorService).get shouldBe "A"
      }

      it("should do flatMap using join") {
        val start = unit(6)
        def mapFn(x: Int) = unit(x*3)

        flatMapUsingJoin(start)(mapFn)(executorService).get shouldBe 18
      }

      it("should do join using flatMap") {
        def nested = unit(unit("B"))
        joinUsingFlatmap(nested)(executorService).get shouldBe "B"
      }
    }
  }
}
