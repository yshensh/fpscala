package fpscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  /**
   * exercise 3.25
   * Write a function size that counts the number of nodes (leaves and branches) in a tree.
   */
  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }


  /**
   * exercise 3.26
   * Write a function maximum that returns the maximum element in a Tree[Int].
   */
  def maximum(t: Tree[Int]): Int =
    t match {
      case Leaf(node) => node
      case Branch(left, right) => maximum(left) max maximum(right)
    }


  /**
   * exercise 3.27
   * Write a function depth that returns the maximum path length from the root of a tree to any leaf.
   */
  def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 0
      case Branch(left, right) => 1+ depth(left) max depth(right)
    }


  /**
   * exercise 3.28
   * Write a function map, analogous to the method of the same name on List, that modifies each element in a tree with a given function.
   */
  def mapForTree[A, B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(node) => Leaf(f(node))
      case Branch(left, right) => Branch(mapForTree(left)(f), mapForTree(right)(f))
    }


  /**
   * exercise 3.29
   * Generalize size, maximum, depth, and map, writing a new function fold that abstracts over their similarities.
   * Reimplement them in terms of this more general function.
   * Like `foldRight` for lists, `fold` receives a "handler" for each of the data constructors of the type,
   * and recursively accumulates some value using these handlers.
   */
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B =
    t match {
      case Leaf(node) => f(node)
      case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(a => 1)(1 + _ + _)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(a => a)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(a => 0)((left, right) => 1 + (left max right))

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))
}
