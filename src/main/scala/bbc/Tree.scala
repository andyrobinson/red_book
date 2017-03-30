package bbc

import scala.annotation.tailrec

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def sizeSimple[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(left,right) => 1 + sizeSimple(left) + sizeSimple(right)
    }
  }

  def size[A](tree: Tree[A]): Int = {

    @tailrec
    def size2(acc: Int, branches: List[Tree[A]]): Int =
      branches match {
        case head :: tail => {
          head match {
            case Leaf(_) => size2(acc + 1, tail)
            case Branch(left, right) => size2(acc + 1, left :: right :: tail)
          }
        }
        case _ => acc
      }

    size2(0, List(tree))
  }

}
