package bbc

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub:String) extends WC


class Exercise10_10_to_10_11 extends Exercise10_1_to_10_3 {

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1,a2) match {
      case (Stub(x), Stub(y)) => {
        (x,y) match {
          case (" ",_) => Part("", 0, y)
          case(_, " ") => Part(x, 0, "")
          case _ => Stub(x + y)
        }
      }
      case (Stub(x), Part(l,n,r)) => Part(x+l, n, r)
      case (Part(l,n,r), Stub(x)) => Part(l,n,r+x)
      case (Part(l1,n1,r1), Part(l2,n2,r2)) => Part(l1,n1+n2+countNonEmpty(r1+l2),r2)
    }

    override def zero: WC = Stub("")
  }

  def divideAndCount(str: String): WC =
    if (str.length > 1) {
      val (str1,str2) = str.splitAt(str.length/2)
      wcMonoid.op(divideAndCount(str1), divideAndCount(str2))
    }
    else Stub(str)

  def countNonEmpty(str: String) = if (str.trim.length > 0) 1 else 0

  def wordCount(str: String): Int = {
    divideAndCount(str) match {
      case Stub(x) => countNonEmpty(x)
      case Part(l,n,r) => n + countNonEmpty(l) +  countNonEmpty(r)
    }
  }

  describe("Word Count Monoid") {
    it("should satisfy the monoid laws") {
      SatisfiesMonoidLaws[WC](wcMonoid, Stub("Super"), Stub("clifragi"), Stub("listic"))
      SatisfiesMonoidLaws[WC](wcMonoid, Stub("th"), Part("e", 3 ,"b"), Stub("oats"))
      SatisfiesMonoidLaws[WC](wcMonoid, Stub("th"), Part("e", 1 ,"m"), Part("an",1,"boats"))
    }
  }

  describe("Word Count") {
    it("should return zero for an empty string") {
      wordCount("") shouldBe 0
    }

    it("should return 1 for a string with one word, ignoring whitespace") {
      wordCount("thing") shouldBe 1
      wordCount(" thing") shouldBe 1
      wordCount("thing ") shouldBe 1
      wordCount(" thing ") shouldBe 1
    }

    it("should count a random string") {
      wordCount(" the old man the boats") shouldBe 5
      wordCount("antidisinstablishmentarianism do") shouldBe 2
      wordCount("the quick brown fox jumped over the lazy dog ") shouldBe 9
      wordCount("  the quick  brown fox jumped     over the  lazy dog ") shouldBe 9
    }

  }
}
