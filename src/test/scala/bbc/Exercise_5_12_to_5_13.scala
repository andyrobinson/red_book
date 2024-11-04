package bbc.stream
import bbc.stream.FpStream.{zipWith, unfold}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Exercise_5_12_to_5_13 extends AnyFunSpec with Matchers {
  describe("5.12") {
    it("can generate fib") {
      val fib10 = unfold(0,1)({case (n,n1) => Some((n,(n1, n + n1)))}).take(10).toList
      fib10 shouldBe List(0,1,1,2,3,5,8,13,21,34)
    }

    it("can generate streams of incrementing integers (from") {
      def unfoldFrom(i: Int): FpStream[Int] = unfold(i)(x => Some(x, x+1))

      unfoldFrom(5).take(3).toList shouldBe List(5,6,7)
      unfoldFrom(-1).take(10).toList shouldBe List(-1,0,1,2,3,4,5,6,7,8)  }
    }

  it("can generate streams of ones") {
    def onesUnfold: FpStream[Int] = unfold(1)(x => Some(1,x))

    onesUnfold.take(3).toList shouldBe List(1,1,1)
    onesUnfold.take(10).toList shouldBe List(1,1,1,1,1,1,1,1,1,1)
  }

  it("can generate streams of the same thing (constant)") {
      def constantFromUnfold[A](item: A): FpStream[A] = unfold(item)(x => Some(x,x))

      constantFromUnfold(5).take(3).toList shouldBe List(5,5,5)
      constantFromUnfold("b").take(10).toList shouldBe List("b","b","b","b","b","b","b","b","b","b")
  }

  describe("5.13") {
    it("can map across a stream") {
      def mapUnfold[A, B](s: FpStream[A])(f: A => B): FpStream[B] =
        unfold(s)(x => x match {
          case Cons(h,t) => Some((f(h()),t()))
          case _ => None
      })

      val stream = FpStream(1,2,3)
      mapUnfold(stream)(x => x + 1).toList shouldBe List(2,3,4)

    }

    it("can take") {
      def takeUnfold[A](s: FpStream[A])(n: Int): FpStream[A] =
        unfold((s, n))(x => x match {
          case (Cons(h,t), n) if n > 1 => Some((h(),(t(),n-1)))
          case (Cons(h,_), 1) => Some((h(), (FpStream.empty[A], 0)))
          case _ => None
        })

      takeUnfold(FpStream.empty[Int])(4) shouldBe FpStream.empty[Int]
      takeUnfold(FpStream(1,2,3,4))(0) shouldBe FpStream.empty[Int]
      takeUnfold(FpStream(1,2,3,4))(2).toList shouldBe FpStream(1,2).toList
      takeUnfold(FpStream(1,2,3,4))(10).toList shouldBe FpStream(1,2,3,4).toList

    }

    it("can takewhile") {

      def takeWhileUnfold[A](s: FpStream[A])(f: A => Boolean): FpStream[A] =
        unfold(s)(x => x match {
          case Cons(h,t) if f(h()) => Some(h(),t())
          case _ => None
        })

      val stream = FpStream("A", "B", "C")

      takeWhileUnfold(FpStream.empty[String])(x => true) shouldBe FpStream.empty[String]
      takeWhileUnfold(stream)(x => true).toList shouldBe stream.toList
      takeWhileUnfold(stream)(x => (x == "A" || x == "B")).toList shouldBe List("A" , "B")

    }

    describe("zip with") {
      def zipSum(s1: FpStream[Int], s2: FpStream[Int]): FpStream[Int] =
        zipWith(s1, s2)(_ + _ )

      it("should produce an empty list with two empty lists") {
        zipSum(Empty, Empty) shouldBe Empty
      }

      it("should give empty if the second list is empty") {
        zipSum(FpStream(4,5,6), Empty) shouldBe Empty
      }

      it("should give empty if the first list is empty") {
        zipSum(Empty, FpStream(4,5,6)) shouldBe Empty
      }

      it("should add together the corresponding numbers in each list") {
        zipSum(FpStream(101, 102, 103), FpStream(4,5,6)).toList shouldBe List(105, 107, 109)
      }

      it("should add together lists and produce a list as short as the shortest if the second is shorter") {
        zipSum(FpStream(101, 102, 103, 104, 105), FpStream(4,5,6)).toList shouldBe List(105, 107, 109)
      }

      it("should add together lists and produce a list as short as the shortest if the first is shorter") {
        zipSum(FpStream(101, 102, 103), FpStream(4,5,6,7,8)).toList shouldBe List(105, 107, 109)
      }

      it("should so something other than numbers") {
        zipWith(FpStream("A", "B", "C"), FpStream("D", "E", "F"))(_ + _).toList shouldBe List("AD", "BE", "CF")
      }

    }

    describe("zip all") {
      it("should zip two empty streams into an empty stream") {
        FpStream.empty[Int].zipAll(FpStream.empty[String]) shouldBe FpStream.empty[(Option[Int],Option[String])]
      }

      it("should zip a stream with an empty stream") {
        val s1 = FpStream(1,2,3)
        s1.zipAll(FpStream.empty[String]).toList shouldBe List((Some(1),None),(Some(2),None),(Some(3),None))
        FpStream.empty[String].zipAll(s1).toList shouldBe List((None,Some(1)),(None,Some(2)),(None,Some(3)))
      }

      it("should zip matching values of equal lists") {
        val s1 = FpStream(1,2)
        val s2 = FpStream("A", "B", "C")
        s1.zipAll(s2).toList shouldBe List((Some(1),Some("A")),(Some(2),Some("B")),(None, Some("C")))

      }
    }
  }
}





