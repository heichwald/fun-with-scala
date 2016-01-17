package tree

import org.specs2.ScalaCheck
import org.specs2.Specification
import scala.util.Random
import Tree.binaryTree


class BinaryTreeSpecTest extends Specification with ScalaCheck {
  def is = s2"""

 This is a specification to check the 'binary tree'

 The tree should work with empty list and be empty tree          $e1
 The tree should work with simple int list                       $e2
 Right subtree is larger and left is smaller                     $e3
 Size of the tree is correct                                     $e4
 Height of the tree is correct when unbalanced                   $e5
 Height of the tree is correct when balanced                     $e6
 Height of the empty tree is correct                             $e7
 Search empty tree                                               $e8
 Search complex tree, present                                    $e9
 Search complex tree, absent                                     $e10
 Delete in the empty tree is should be empty tree                $e11
 Deletion right in a simple tree                                 $e12
 Deletion in a various trees                                     $e13
 Deletion non existing item in a various trees                   $e14
                                                                 """

  def e1 = binaryTree(List[Int]()) must_== Empty
  def e2 = binaryTree(List[Int](3, 1, 2)) must_==
    Node(Element(3, 1), Node(Element(1, 1), Empty, Node(Element(2, 1),
      Empty, Empty)), Empty)

  def isWellformed(t: Tree[Int]): Boolean = t match {
    case Empty => true
    case Node(el, left, right) => {
      val isLeftValid = left match {
        case Empty         => true
        case Node(ell, _, _) => el.value > ell.value && isWellformed(left)
      }
      val isRightValid = right match {
        case Empty         => true
        case Node(elr, _, _) => el.value < elr.value && isWellformed(right)
      }
      isLeftValid && isRightValid
    }
  }

  /*
   * Generate automatically random lists
   */
  def e3 = prop {
    (l: List[Int]) =>
    {
      isWellformed(binaryTree(l)) must beTrue
    }
  }

  def e4 = prop {
    (l: List[Int]) =>
    {
      binaryTree(l).size must_== l.distinct.size
    }
  }

  def e5 = binaryTree(List[Int](1, 2, 3)).height must_== 2
  def e6 = binaryTree(List[Int](2, 1, 3)).height must_== 1
  def e7 = binaryTree(List[Int]()).height must_== -1
  def e8 = binaryTree(List[Int]()) search 5 must beFalse
  def e9 = binaryTree(List[Int](1, 2, 3, 56, 6, 8, 15)) search 9 must beFalse
  def e10 = binaryTree(List[Int](1, 2, 3, 56, 6, 8, 15)) search 8 must beTrue
  def e11 = binaryTree(List[Int]()) delete 3 must_== Empty
  def e12 = binaryTree(List[Int](1, 2, 3)) delete 3 must_==
    Node(Element(1, 1), Empty, Node(Element(2, 1), Empty, Empty))
  def e13 = prop {
    (l: List[Int]) =>
      l match {
        case Nil => binaryTree(l) delete 3 must_== Empty
        case _ =>
          val lunique = l.distinct
          val tree = binaryTree(l)
          val sizeBefore = tree.size
          val toDelete = (Random shuffle lunique).head
          val afterDeletionTree = tree delete toDelete
          afterDeletionTree.size == sizeBefore - 1 &&
            isWellformed(afterDeletionTree) must beTrue
      }
  }
  def e14 = prop {
    (l: List[Int]) =>
    {
      val tree = binaryTree(l)
      val sizeBefore = tree.size
      val s = Stream.continually(Random.nextInt)
      val n = (s dropWhile { l.contains(_) }).head
      tree delete n
      tree.size == sizeBefore && isWellformed(tree) must beTrue
    }
  }
}
