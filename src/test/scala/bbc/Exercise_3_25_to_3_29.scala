package bbc

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Exercise_3_25_to_3_29 extends AnyFunSpec with Matchers {
  describe("Tree size") {
    it("should return 1 for a leaf") {
      Tree.size(Leaf(1)) shouldBe 1
    }

    it("should return 3 for a tree with 1 branch and 2 leaves") {
      Tree.size(Branch(Leaf(1),Leaf(2))) shouldBe 3

    }

    it("should return 5 for a tree with 2 branch and 3 leaves") {
      Tree.size(Branch(Branch(Leaf(1),Leaf(3)),Leaf(2))) shouldBe 5
    }
  }

  describe("Maximum") {
    it("should return the value of a single node") {
      Tree.max(Leaf(99)) shouldBe 99
    }

    it("should return the greater of two leaves") {
      Tree.max(Branch(Leaf(46),Leaf(53))) shouldBe 53
    }

    it("should return the greater of two tree") {
      Tree.max(Branch(Branch(Leaf(46),Leaf(53)),Branch(Leaf(65), Leaf(89)))) shouldBe 89
    }
  }

  describe("Map") {

    it("should apply the function to a leaf") {
      Tree.map(Leaf(4))(_ + 1) shouldBe Leaf(5)
    }

    it("should apply the function to every element of a tree") {
      Tree.map(Branch(Branch(Leaf(46),Leaf(53)),Branch(Leaf(65), Leaf(89))))(_ + 2) shouldBe
        Branch(Branch(Leaf(48),Leaf(55)),Branch(Leaf(67), Leaf(91)))
    }

  }

}
