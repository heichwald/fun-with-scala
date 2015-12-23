package tree

/**
  * Defines a tree element has having a value and a count to avoid duplicates in the tree 
 */
case class Element[T](val value: T, count: Int = 1) {
  override def toString = s"(${value},$count)"
}

/**
  * A tree definition, support for add, delete, search, size, height and pretty print 
 */
sealed abstract class Tree[T](implicit ordering: Ordering[T]) {
  val size: Int
  val height: Int

  def add(a: T): Tree[T]

  def search(a: T): Boolean

  def delete(a: T): Tree[T]

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
  def binaryTree[T](l: List[T])(implicit ordering: Ordering[T]) =
    l.foldLeft(Empty(): Tree[T])((b, a) => b.add(a))
}

/**
  * Act a stopper for the tree branches
 */
sealed case class Empty[T](implicit ordering: Ordering[T]) extends Tree[T] {
  val size = 0
  val height = -1

  def add(a: T) = Node(Element(a), Empty(), Empty())

  def search(a: T) = false

  def delete(a: T) = Empty()

  def merge(tree: Tree[T]) = tree

  protected def printLevel0() = print("E")
    
  protected def printOtherLevel(level: Int) = {
    printGivenLevel(level)
    print(" ")
    printGivenLevel(level)
  }
  
  override def equals(o: Any) = o.isInstanceOf[Empty[T]]

  override def hashCode = 31
}

sealed case class Node[T]
    (el: Element[T], left: Tree[T], right: Tree[T])
    (implicit ordering: Ordering[T])
  extends Tree[T] {
  import ordering._

  val size = 1 + left.size + right.size
  val height = 1 + math.max(left.height, right.height)

  /**
    * If value already exists then increment count value
    * If bigger then continue in the right subtree 
    * else in the left subtree
   */
  def add(a: T) = {
    if (a == el.value) Node(Element(el.value, el.count + 1), left, right)
    else if (a > el.value) Node(el, left, right.add(a))
    else Node(el, left.add(a), right)
  }

  /**
    * Found if same value otherwise
    * if bigger, search in right subtree
    * else search in left subtree
   */
  def search(a: T) = 
    if (a == el.value) true
    else if (a < el.value) left.search(a)
    else right.search(a)

  /**
    * Pop the maximum element from the right subtree
    * and return the max value and the new subtree   
   */
  def popMaximum: (Element[T], Tree[T]) = right match {
    case Empty() => (el, left)
    case nRight: Node[T] =>
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
  def delete(a: T) = {     
    if (el.value == a) {
      left match {
        case Empty()               => right
        case _ if right == Empty() => left
        case nLeft: Node[T]        =>
          val (max, newLeft) = nLeft.popMaximum
          Node(max, newLeft, right)
      }
    } else if (a < el.value) { 
      Node(el, left.delete(a), right)
    } else {
      Node(el, left, right.delete(a))
    }
  }

  protected def printLevel0() = print(el)
    
  protected def printOtherLevel(level: Int) = {
    left.printGivenLevel(level)
    print(" ")
    right.printGivenLevel(level)
  }
  
  /**
    * Same element (value and count) and same left and right subtrees
   */
  override def equals(o: Any) = o match {
    case Node(el2, left2, right2) =>
      el == el2 && left == left2 && right == right2
    case _                        => false
  }

  override def hashCode = 31 * (el.hashCode + left.hashCode + right.hashCode)
}

