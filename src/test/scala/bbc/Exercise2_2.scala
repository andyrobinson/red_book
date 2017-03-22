package bbc

import org.scalatest.{FunSpec, Matchers}

import scala.annotation.tailrec

class Exercise2_2 extends FunSpec with Matchers {

  describe("Is sorted") {

    def compareInt(a:Int, b:Int): Boolean = a <= b
    def compareString(a:String, b: String): Boolean = a <= b

    @tailrec
    def isSorted[A] (arr: Array[A], ordered: (A,A) => Boolean): Boolean = {
      (arr.length <= 1) || { val tail = arr.tail; ordered(arr.head,tail.head) && isSorted(tail,ordered) }
    }

    @tailrec
    def isSorted2[A] (arr: Array[A], ordered: (A,A) => Boolean): Boolean = {
      arr match {
        case Array(first, second, _*)  => ordered(first, second) && isSorted2 (arr.tail, ordered)
        case _ => true
      }
    }

    def isSortedFold[A] (arr: Array[A], ordered: (A,A) => Boolean): Boolean = {
      arr.foldLeft((true,arr.headOption)){ case ((acc,lastValue),value) =>
        (acc && ordered(lastValue.get,value), Some(value))}._1
    }

    it ("should report an empty array as sorted") {
      isSorted(Array.empty[Int], compareInt) shouldBe true
    }

    it("should report a single element array as sorted") {
      isSorted(Array("A"),compareString) shouldBe true
    }

    it("should report an unsorted array as not sorted") {
      isSorted(Array(3,1,2), compareInt) shouldBe false
    }

    it("should report a sorted array as sorted") {
      isSorted(Array(1,2,3,4), compareInt) shouldBe true
    }

    it("should do strings as well as numbers") {
      isSorted(Array("A","Z","a","z"),compareString) shouldBe true
    }
  }

  describe("Is sorted using fold") {

  }
}
