//package bbc
//
//case class Prop2(run: Prop.TestCases => Prop.Result)  {
//
//}
//
//object Prop {
//  type SuccessCount = Int
//  type TestCases = Int
//  type MaxSize = Int
//  type FailedCase = String
//
//  sealed trait Result {
//    def isFalsified: Boolean
//  }
//
//  case object Passed extends Result {
//    def isFalsified = false
//  }
//
//  case class Falsified(failure: FailedCase,
//                       successes: SuccessCount) extends Result {
//    def isFalsified = true
//  }
//
//  case object Proved extends Result {
//    def isFalsified = false
//  }
//
//  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
//    (n, rng) =>
//      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
//        case (a, i) => try {
//          if (f(a)) Passed else Falsified(a.toString, i)
//        } catch {
//          case e: Exception => Falsified(buildMsg(a, e), i)
//        }
//      }.find(_.isFalsified).getOrElse(Passed)
//  }
//}
