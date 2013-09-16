package com.github.cb372.collection

import scala.annotation.tailrec

/**
 * A UnionFind node correspoding to a given element
 */
private[collection] case class Node[T](elem: T, rank: Int, parent: T)

/**
 * Immutable Union-Find implementation with union-by-rank and path compression.
 */
case class UnionFind[T] private (nodes: Map[T, Node[T]], count: Int) {

  /*
   * Merge the groups containing the given two elements.
   *
   * @return an updated structure
   */
  def union(e1: T, e2: T): UnionFind[T] = {
    require(nodes.contains(e1) && nodes.contains(e2), "Only elements previously added can be unioned")

    val (rootElem1, uf1) = this.find(e1)
    val (rootElem2, uf2) = uf1.find(e2)
    val (root1: Node[T], root2: Node[T]) = (nodes(rootElem1.get), nodes(rootElem2.get))
    (root1, root2) match {
      // both elements already in same group: nothing to do
      case (n1, n2) if n1 == n2 => this

      // rank1 > rank2 -> n1 becomes parent of n2, ranks stay the same
      case (n1 @ Node(e1, rank1, _), n2 @ Node(_, rank2, _)) if rank1 > rank2 =>
        new UnionFind(uf2.nodes + (e2 -> n2.copy(parent = e1)), 
                      count - 1)

      // rank1 < rank2 -> opposite of previous case
      case (n1 @ Node(_, rank1, _), n2 @ Node(e2, rank2, _)) if rank1 < rank2 =>
        new UnionFind(uf2.nodes + (e1 -> n1.copy(parent = e2)), 
                      count - 1)

        // equal ranks -> arbitrarily choose n1 as root and increase its rank by 1
      case (n1 @ Node(_, rank1, _), n2 @ Node(_, rank2, _)) /*if rank1 == rank2*/ =>
        new UnionFind(uf2.nodes + (e1 -> n1.copy(rank = rank1 + 1))
                            + (e2 -> n2.copy(parent = e1)),
                      count - 1)
    } 
  }

  /**
   * Find the group to which the given element belongs.
   * Also returns an updated, possibly optimized, UnionFind structure.
   */
  def find(elem: T): (Option[T], UnionFind[T]) = {
    nodes.get(elem).fold[(Option[T], UnionFind[T])] {
      (None, this)
    } { node =>
      val ns = pathToRoot(node)
      val root = ns.head

      // path compression: redirect all parent pointers to point directly to root
      val updatedNodes = ns.foldLeft (nodes) { (nodes, n) =>
        nodes + (n.elem -> n.copy(parent = root.elem))
      }

      (Some(root.elem), new UnionFind[T](updatedNodes, count))
    }
  }

  /**
   * The number of distinct groups in the structure.
   */
  def size: Int = count

  private def pathToRoot(node: Node[T]): List[Node[T]] = {
    @tailrec
    def path(ns: List[Node[T]]): List[Node[T]] = {
      val n = ns.head
      n.parent match {
        case n.elem => ns
        case p => path(nodes(p) :: ns)
      }
    } 
    path(List(node))
  }

}

object UnionFind {
  private def node[T](elem: T): Node[T] = Node(elem, 0, elem)

  /**
   * Create a UnionFind structure containing the given elements.
   * Each element is given its own singleton group.
   */
  def apply[T](elements: T*): UnionFind[T] = {
    val nodes = Map(elements.map{e => (e -> node(e))} : _*)
    new UnionFind[T](nodes, elements.size)
  }
}
