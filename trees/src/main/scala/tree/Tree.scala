package tree

/**
  * Defines a tree element has having a value and a count to avoid duplicates in the tree
  */
case class Element[+A](val value: A, count: Int = 1) {
  override def toString = s"(${value},$count)"
}

/**
  * A tree definition, support for add, delete, search, size, height and pretty print
  */
sealed abstract class Tree[+A] {
  val size: Int
  val height: Int

  def add[B >: A](b: B)(implicit ordering: Ordering[B]): Tree[B]

  def search[B >: A](b: B)(implicit ordering: Ordering[B]): Boolean

  def delete[B >: A](b: B)(implicit ordering: Ordering[B]): Tree[A]

  protected def printLevel0(): Unit

  protected def printOtherLevel(subLevel: Int): Unit

  private[tree] def printGivenLevel(level: Int): Unit = level match {
    case 0 => printLevel0()
    case _ => printOtherLevel(level - 1)
  }

  def prettyPrint(): Unit = (0 to height + 1) foreach { h =>
    printGivenLevel(h)
    println()
  }
}

object Tree {
  def binaryTree[A](l: List[A])(implicit ordering: Ordering[A]) = l.foldLeft(Empty: Tree[A])((b, a) => b.add(a))
}

/**
  * Act a stopper for the tree branches
  */
object Empty extends Tree[Nothing] {
  val size = 0
  val height = -1

  def add[B >: Nothing](b: B)(implicit ordering: Ordering[B]) = Node(Element(b), Empty, Empty)

  def search[B >: Nothing](b: B)(implicit ordering: Ordering[B]) = false

  def delete[B >: Nothing](b: B)(implicit ordering: Ordering[B]) = Empty

  protected def printLevel0() = print("E")

  protected def printOtherLevel(level: Int) = {
    printGivenLevel(level)
    print(" ")
    printGivenLevel(level)
  }
}

sealed case class Node[+A] (el: Element[A], left: Tree[A], right: Tree[A]) extends Tree[A] {
  val size = 1 + left.size + right.size
  val height = 1 + math.max(left.height, right.height)

  /**
    * If value already exists then increment count value
    * If bigger then continue in the right subtree
    * else in the left subtree
    */
  def add[B >: A](b: B)(implicit ordering: Ordering[B]) = {
    import ordering._
    if (b == el.value) Node(Element(el.value, el.count + 1), left, right)
    else if (b > el.value) Node(el, left, right.add(b))
    else Node(el, left.add(b), right)
  }

  /**
    * Found if same value otherwise
    * if bigger, search in right subtree
    * else search in left subtree
    */
  def search[B >: A](b: B)(implicit ordering: Ordering[B]) = {
    import ordering._
    if (b == el.value) true
    else if (b < el.value) left.search(b)
    else right.search(b)
  }

  /**
    * Pop the maximum element from the right subtree
    * and return the max value and the new subtree
    */
  def popMaximum: (Element[A], Tree[A]) = right match {
    case Empty => (el, left)
    case nRight: Node[A] =>
      val (max, t) = nRight.popMaximum
      (max, Node(el, left, t))
  }

  /**
    * If found and no sub trees, then just replace it by Empty
    * If only left or right subtree, then just replace it by the subtree
    * Else find maximum in the left subtree and replace the element `a` by
    * this maximum. The maximum is removed from the left subtree
    * Note that another implementation could be to decrement first the count if > 1
    */
  def delete[B >: A](b: B)(implicit ordering: Ordering[B]): Tree[A] = {
    import ordering._
    if (el.value == b) {
      left match {
        case Empty               => right
        case _ if right == Empty => left
        case nLeft: Node[A]        =>
          val (max, newLeft) = nLeft.popMaximum
          Node(max, newLeft, right)
      }
    } else if (b < el.value) {
      Node(el, left.delete(b), right)
    } else {
      Node(el, left, right.delete(b))
    }
  }

  protected def printLevel0() = print(el)

  protected def printOtherLevel(level: Int) = {
    left.printGivenLevel(level)
    print(" ")
    right.printGivenLevel(level)
  }
}
