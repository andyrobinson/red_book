package bbc

import org.scalatest.{FunSpec, Matchers}

class Exercise_3_25_to_3_29 extends FunSpec with Matchers {
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

}
