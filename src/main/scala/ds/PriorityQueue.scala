package ds

import scala.collection.mutable.ArrayBuffer
import scala.collection.generic.{CanBuildFrom, OrderedTraversableFactory, GenericOrderedCompanion, GenericOrderedTraversableTemplate}
import scala.collection._
import scala.Iterator

/**
 * Abstract class representing a PriorityQueue which can be used thus:
 *
 * @example {{{
 *  // Make a PriorityQueue via the companion object factory
 *  val pq = PriorityQueue(4,3,2,1,5)
 *
 *  val min = pq.findMin // 1
 *  val secondMin = pq.deleteMin.findMin // 2
 *  val npq = pq + 0
 *  val npq.findMin // 0
 *  }}}
 * @param ord Ordering of data.
 * @tparam A Type of data.
 */
abstract class PriorityQueue[A](implicit val ord: Ordering[A])
  extends Iterable[A]
  with GenericOrderedTraversableTemplate[A, PriorityQueue]
  with IterableLike[A, PriorityQueue[A]] {

  /**
   * Adds a new item to the PriorityQueue.
   * @param x Item to add
   * @return New PriorityQueue which has this item added
   */
  def +(x: A) : PriorityQueue[A]

  /**
   * Finds the minimum item.
   * @return the minimum item
   */
  def findMin: A

  /**
   * Deletes the minimum item.
   *
   * @return New PriorityQueue which has the min item deleted.
   */
  def deleteMin(): PriorityQueue[A]

  /**
   * Merges two PriorityQueue together.
   *
   * @param that the PriorityQueue to merge
   * @return merged PriorityQueue
   */
  def meld(that: PriorityQueue[A]) : PriorityQueue[A]

  override def orderedCompanion: GenericOrderedCompanion[PriorityQueue] = PriorityQueue
  override protected[this] def newBuilder: mutable.Builder[A, PriorityQueue[A]] = PriorityQueue.newBuilder
}

object PriorityQueue extends OrderedTraversableFactory[PriorityQueue] {
  // println("In object PriorityQueue TOP")
  // Methods related to scala collections
  override def newBuilder[A](implicit ord: Ordering[A]): mutable.Builder[A, PriorityQueue[A]] = {
    // println("In object PriorityQueue def newBuilder")
    new ArrayBuffer[A] mapResult { xs =>
      // println("In def newBuilder, creating ArrayBuffer, xs: " + xs)
      val bq = xs.foldLeft(BinomialQueue[A](Nil))((t, x) => t + x)
      // println("In def newBuilder, mapped xs into bq, bq: " + bq)
      bq
    }
  }

  implicit def canBuildFrom[A](implicit ord: Ordering[A])
  	: CanBuildFrom[Coll, A, PriorityQueue[A]] = new GenericCanBuildFrom[A]
}

/**
 * A class representing a Node in the Binomial queue. Not intended to be used directly.
 * @param data The actual value stored in the Node.
 * @param rank Rank of the node.
 * @param children List of children. // ordered by data
 * @param ord Ordering of data.
 * @tparam A Type of data.
 */
private[ds] case class Node[A](data: A, rank: Int = 0, children: List[Node[A]] = Nil)
                          (implicit val ord: Ordering[A]) extends Ordered[Node[A]] {

  // println("In Node constructor, data:" + data + " rank:" + rank + " children:" + children)
  
  /**
   * Links this node with another one and appropriately rearranges things.
   * @param other the node to link
   * @return Node with this and other linked.
   */
  /* RW: I'm going to state it more precicely:
   * Trees that are found to be the same size within the binomial heap are joined into
   * one larger tree (double the size of those being joined). Node.data (min) is the 
   * basis of priority for the PriorityQueue.The one with the lesser data value consumes 
   * the other. The new node, doubled is size, increments it's rank (and number of children)
   * by 1.
   * 
   * One thing that is notable structurally is that the child node is prepended (cons)
   * to the Node.children list. This means that the BinomialQueue.nodes List is ordered 
   * in ascending order according to rank (Do I know this to be true? Yes, see meldLists),
   * while the Node.children List is ordered in descending rank. (See
   * diagram http://en.wikipedia.org/wiki/Binomial_heap#/media/File:Binomial_Trees.svg).
   */ 
  def link(other: Node[A]) = {
    // println("In Node def link, this.data:" + this.data + " other.data:" + other.data)
    
    /* data is the basis of priority for the PriorityQueue. The node with the lesser data
     * value becomes the root and the other becomes its child.
     */
    if (ord.compare(data, other.data) < 0) { 
      val new_n = Node(data, rank+1, other :: children)
      // println("In Node def link, creating new Node with this.data:" + this.data + ", cons other:" + other + " to this.children: " + new_n)
      new_n
    }
    else {
      val new_n = Node(other.data, other.rank+1, this :: other.children)
      // println("In Node def link, creating new Node with other.data:" + other.data + ", cons this:" + this + " to other.children: " + new_n)
      new_n
    }
  }

  def toList : List[A] = data :: children.flatMap(_.toList)

  override def compare(that: Node[A]): Int = ord.compare(data, that.data)
} // end Node

/**
 * This class implements a PriorityQueue using Binomial heaps.
 * @param nodes The forest of Nodes
 * @param ord Ordering of data // Clarification: ordering of nodes is by rank! See meldLists
 * @tparam A Type of data
 */
private[ds] final case class BinomialQueue[A] (nodes: List[Node[A]])(implicit override val ord: Ordering[A])
  extends PriorityQueue[A] {
  
  // println("In BinomialQueue constructor, nodes: " + nodes)

  def +(x: A): BinomialQueue[A] = {
    // println("In BinomialQueue def +, calling BinomialQueue(insertNode...)...")
    val bq = BinomialQueue(insertNode(Node(x), nodes)) // assuming nodes are implicitly ordered by rank
    // println("In BinomialQueue def +, bq: " + bq)
    bq
  }

  def findMin: A = {
    // println("In BinomialQueue def findMin. nodes is just a List, so implicit min is easy...")
    nodes.min.data // min comes from TraversableOnce. See also GenericOrderedTraversableTemplate, OrderedTraversableFactory, GenericOrderedCompanion, Ordering, Ordered
  }

  def deleteMin: PriorityQueue[A] = {
    val minNode = nodes.min
    println("In BinomialQueue def deleteMin, minNode: " + minNode +
        "\n" + "calling BinomialQueue meldLists filter out minNode")
    /* 
     * Why is reverse called on children? We're removing the min node, but not
     * its children, so they have to be melded back into the BinomialQueue.nodes
     * List. As I've previously pointed out (meldList comments above), the 
     * Node.children List is ordered in descending rank whereas the BinomialQueue.nodes 
     * list is ordered in ascending rank, hense the reverse of minNode.children. 
     */
    BinomialQueue(meldLists(nodes.filter(_ != minNode), minNode.children.reverse))
  }

  def meld(that: PriorityQueue[A]): PriorityQueue[A] = {
    // println("In BinomialQueue def meld," +
        //"\n" + "this: " + this +
        //"\n" + "that: " + that)
    (this, that) match {
	    case (BinomialQueue(Nil), q) => q
	    case (q, BinomialQueue(Nil)) => q
	    case (BinomialQueue(thisList), BinomialQueue(thatList)) => {
	      // println("In BinomialQueue def meld, calling BinomialQueue meldLists...")
	      BinomialQueue(meldLists(thisList, thatList))
	    }
	}
  }

  // Methods related to scala collections
  override def isEmpty: Boolean = nodes.isEmpty
  override def iterator: Iterator[A] = nodes.flatMap(_.toList).iterator

  /* Each list is a binomial heap. Each Node in a List is a binomial tree of sizes
   * 2^0, 2^1, 2^2, 2^3 ... 2^k. The binary exponent corresponds to the the tree's 
   * rank (or depth) as well as the number of its children (i.e. Node.children).
   * Rank (size, number of children) is the basis for ordering of BinomialQueue.nodes. 
   */
  // Recursive
  private def meldLists[T](q1: List[Node[T]], q2: List[Node[T]]): List[Node[T]] = {
    // println("In BinomialQueue def meldLists, q1: " + q1 + " | q2: " + q2)
    (q1, q2) match {
	    case (Nil, q) => q // convergence condition, stops recursion
	    case (q, Nil) => q // convergence condition, stops recursion
	    case (x :: xs, y :: ys) => {
	    		/*println("In BinomialQueue def meldLists, case(x :: xs, y :: ys)" +
	    		    "\n" + "recursing meldLists based on x: " + x +
	    		    "\n" + "and y: " + y)*/
	    		// ordering trees in ascending size (i.e. rank, or number of children)
	    		if (x.rank < y.rank) x :: meldLists(xs, y :: ys) // recurse
		    else if (x.rank > y.rank) y :: meldLists(x :: xs, ys) // recurse
		    else {
		      // both trees are the same size, so they can be joined
		      println("In BinomialQueue def meldLists list match case (x :: xs, y :: ys)," +
	            "\n" + "calling insertNode(x:" + x + ".link(y:" + y + "), meldLists(xs, ys))")
		      insertNode(x.link(y), meldLists(xs, ys))
		    }
	    }
	}
  }

  /* Recursive. The list has been ordered by rank (assuming call from meldLists. Actually there is
   * one other call from BinomialQueue def + with implicit ordering).
   */
  private def insertNode[T](n: Node[T], list: List[Node[T]]): List[Node[T]] = {
    println("In BinomialQueue def insertNode TOP, n: " + n + " | list: " + list)
    
    list match {
    		// Easy, inserted node is first and only node in list...
	    case Nil => {
	      println("In BinomialQueue def insertNode list match case Nil, returning List(n:" + n + ")...")
	      List(n)
	    }
	    case x :: xs => {
	      /* Also easy: if inserted node is smaller than the head of the list, it becomes the new head.
	       * Otherwise, 
	       */
	      println("In BinomialQueue def insertNode list match case x :: xs," +
	          "\n" + "n.rank:" + n.rank + " x.rank:" + x.rank)
	      if (n.rank < x.rank) {
	        println("In BinomialQueue def insertNode list match case x :: xs," +
	            "\n" + "cons list as: " + n + " :: " + x + " :: " + xs)
	        n :: x :: xs
	      }
	      else {
	        /* Oh, this confuses me: I get that if n.rank is smaller than rank of the current head
	         * then n should become head, but on what basis can we assume that if n.rank is not
	         * less that x.rank that they therefor must be the same size and never larger? 
	         * (link only joins trees/heaps of the same size (rank)). I guess it is that the inserted
	         * node, coming from meldLists, has had its rank increased by 1 and is now being compared
	         * with the next node in the list. So the inserted node, if not less than the next node's,
	         * rank, must therefor be equal to it. */
	        println("In BinomialQueue def insertNode list match case x :: xs," +
	            "\n" + "calling insertNode(x:" + x + ".link(n:" + n + "), " + xs + ")")
	        insertNode(x.link(n), xs) // recurse
	      }
	    }
	}
  }
} // end BinomialQueue








