package bbc

import scala.annotation.tailrec

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def fold[A,B](tree: Tree[A])(f: A => B)(fb: (B,B) => B): B = tree match {
    case Leaf(x) => f(x)
    case Branch(left,right) => fb(fold(left)(f)(fb),fold(right)(f)(fb))
  }

  def map[A,B](tree: Tree[A])(fn: A => B):Tree[B] =
    fold(tree)(x => Leaf(fn(x)): Tree[B])((l,r) => Branch(l,r))

  def max(tree: Tree[Int]): Int =
    fold(tree)(x => x)((l,r) => l max r)

  def size[A](tree: Tree[A]): Int =
    fold(tree)(x => 1)((l,r) => 1 + l + r)

  def map1[A,B](tree: Tree[A])(fn: A => B):Tree[B] = tree match {
    case Leaf(x) => Leaf(fn(x))
    case Branch(left,right) => Branch(map(left)(fn),map(right)(fn))
  }

  def max1(tree: Tree[Int]): Int = tree match {
    case Leaf(x) => x
    case Branch(left,right) => max(left) max max(right)
  }

  def sizeSimple[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(left,right) => 1 + sizeSimple(left) + sizeSimple(right)
    }
  }

  def size1[A](tree: Tree[A]): Int = {

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
