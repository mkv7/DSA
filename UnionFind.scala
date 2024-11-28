// Scala 3 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Tommi Junttila, Aalto University
 * This file is only for your personal use in the Aalto course CS-A1140.
 * Distribution of any parts of this file in any form,
 * including posting to public or shared forums,
 * storing in public or shared repositories,
 * is *not allowed*,
 * and constitutes a violation of the code of conduct of the course.
 * In addition, entering any parts of this file or the assignment instructions
 * into an AI tool, or to otherwise distributing them to external parties,
 * is *not allowed*.
 */

package unionFind

import scala.collection.mutable

object solver:
  /**
   * Given vertices {0,...,nofVertices-1} and weighted edges between them,
   * each edge of weight w between vertices v1 and v2 given as a triple
   * (v1, w, v2), find the largest weight W such that if we only consider
   * the edges of weight W or more, the graph stays connected (i.e., we can get
   * from any vertex to each other vertex).
   * Returns None if no such weight exists, i.e., if the graph is not connected
   * even if we consider all the edges given.
   * Provided that the union-find data structure is properly implemented and
   * includes either ranks or path compression (or both),
   * the running time of the algorithm is
   * O(nofVertices + |edges|*(log(|edges|) + log(nofVertices)))
   * on average.
   */
  def solve(nofVertices: Int, edges: collection.Seq[(Int, Int, Int)]): Option[Int] =
    // Input validation
    require(nofVertices >= 1)
    // Observe that the argument "edges" is a Seq, and can thus be,
    // for instance, a List. Therefore random access of the form "edges(i)"
    // to it can be costly.
    edges.foreach({case (vertex1, weight, vertex2) =>
      require(0 <= vertex1 && vertex1 < nofVertices &&
              0 <= vertex2 && vertex2 < nofVertices &&
              weight >= 0)})
    // The sets of vertices
    if (nofVertices == 1) then return None

    val sets = new UnionFind[Int]()
    for i <- 0 until nofVertices do sets.makeSet(i)
    
    val sortedEdges = edges.sortBy(-_._2)
    
    for (v1, w, v2) <- sortedEdges do
      sets.union(v1, v2)
      if sets.nofSets == 1 then return Some(w)
     
    None
  end solve
end solver



class UnionFind[E]:
  protected val parent = new scala.collection.mutable.HashMap[E, E]()
  protected val rank = new scala.collection.mutable.HashMap[E, Int]()
  protected var _nofSets = 0




  /**
   * Introduce a new element in this disjoint sets data structure and
   * put it into the set consisting only of the element itself.
   * Does nothing if the element is already in the disjoint sets data structure.
   * Returns true if the element was inserted (i.e., was not already in
   * the data structure), false otherwise.
   * A constant-time operation (actually only on average if hash map
   * searches and insertions are used in the code).
   */
  def makeSet(element: E): Boolean =
    if (parent.contains(element)) then false
    else
      parent(element) = element
      rank(element) = 0
      _nofSets += 1
      true
  end makeSet


  /**
   * Get the representative element of the given element
   * in the current disjoint sets data structure.
   * Two elements are in the same set if and only if their representatives
   * are the same.
   * The representatives may change during union operations and
   * thus it is *not* safe to use previously calculated representatives
   * after an union operation has been performed.
   * Throws an exception if the element has not been introduced earlier
   * with makeSet.
   * An O(log n) operation on average,
   * where n is the number of elements in all the sets.
   */
  def findSet(element: E): E =
    if (parent(element) != element) then
      parent(element) = findSet(parent(element))
    parent(element)
  end findSet


  /**
   * Merge (i.e., make union of) the sets containing the elements
   * element1 and element2.
   * An O(log n) operation, where n is the number of elements in all the sets.
   */
  def union(element1: E, element2: E): Unit =
    val first = findSet(element1)
    val second = findSet(element2)

    if (first != second) then
      if (rank(first) > rank(second)) then
        parent(second) = first
      else if (rank(first) < rank(second)) then
        parent(first) = second
      else
        parent(second) = first
        rank(first) += 1

      _nofSets -= 1
  end union


  /** Get the number of elements in this disjoint sets data structure. */
  def nofElements: Int = parent.size


  /** Get the number of sets in this disjoint sets data structure. */
  def nofSets: Int = _nofSets


end UnionFind
