package saki.util

import scala.collection.mutable

/**
 * A class representing a graph data structure.
 *
 * @param adjacencyList The adjacency list representing the graph.
 * @param isDirected    A boolean indicating whether the graph is directed.
 * @tparam T The type of the vertices in the graph.
 */
class Graph[T](
  val isDirected: Boolean = true,
  private val adjacencyList: Map[T, Set[T]] = Map.empty[T, Set[T]],
) {

  /**
   * Adds a vertex to the graph, returning a new graph.
   *
   * @param vertex The vertex to add.
   * @return A new graph with the added vertex.
   */
  def addVertex(vertex: T): Graph[T] = {
    if adjacencyList.contains(vertex) then this else {
      Graph(isDirected, adjacencyList + (vertex -> Set.empty[T]))
    }
  }

  /**
   * Adds an edge between two vertices in the graph, returning a new graph.
   *
   * @param v1 The first vertex.
   * @param v2 The second vertex.
   * @return A new graph with the added edge.
   */
  def addEdge(v1: T, v2: T): Graph[T] = {
    val updatedGraph = addVertex(v1).addVertex(v2)
    val updatedAdjList = updatedGraph.adjacencyList + (v1 -> (updatedGraph.adjacencyList(v1) + v2))
    val finalAdjList = if isDirected then updatedAdjList else {
      updatedAdjList + (v2 -> (updatedAdjList.getOrElse(v2, Set.empty[T]) + v1))
    }
    Graph(isDirected, finalAdjList)
  }

  /**
   * Retrieves the neighbors of a given vertex.
   *
   * @param vertex The vertex whose neighbors are to be retrieved.
   * @return A set of neighbors of the given vertex.
   */
  def neighbors(vertex: T): Set[T] = adjacencyList.getOrElse(vertex, Set.empty[T])

  /**
   * Checks if the graph contains a given vertex.
   *
   * @param vertex The vertex to check.
   * @return True if the vertex is in the graph, false otherwise.
   */
  def containsVertex(vertex: T): Boolean = adjacencyList.contains(vertex)

  /**
   * Checks if there is an edge between two vertices.
   *
   * @param v1 The first vertex.
   * @param v2 The second vertex.
   * @return True if there is an edge from v1 to v2, false otherwise.
   */
  def hasEdge(v1: T, v2: T): Boolean = neighbors(v1).contains(v2)

  /**
   * Retrieves all vertices in the graph.
   *
   * @return An iterable of all vertices in the graph.
   */
  def vertices: Iterable[T] = adjacencyList.keys

  /**
   * Retrieves all edges in the graph.
   *
   * @return A list of all edges in the graph.
   */
  def edges: List[(T, T)] = adjacencyList.flatMap {
    case (v1, neighbors) => neighbors.map(v2 => (v1, v2))
  }.toList

  /**
   * Checks if a specific node is part of a cycle.
   *
   * @param node The node to check.
   * @return True if the node is part of a cycle, false otherwise.
   */
  def isInCycle(node: T): Boolean = {

    // Recursive DFS function that passes updated visited and recursion stack immutably
    def dfs(vertex: T, visited: Set[T]): Boolean = {
      if (vertex == node) true // A cycle is detected
      else if (visited.contains(vertex)) false // Already processed, no cycle here
      else {
        // Explore neighbors recursively with updated visited and recStack sets
        val newVisited = visited + vertex
        // If any neighbor leads to a cycle, return true
        neighbors(vertex).exists(neighbor => dfs(neighbor, newVisited))
      }
    }

    // Start DFS with empty visited
    if !containsVertex(node) then false else {
      neighbors(node).exists(neighbor => dfs(neighbor, Set.empty[T]))
    }
  }

  /**
   * Checks if a specific node is part of a cycle.
   * @param node The node to check.
   * @return True if the node is part of a cycle, false otherwise.
   */
  def isSelfLoop(node: T): Boolean = neighbors(node).contains(node)

  /**
   * Retrieves the reachable set of a given node.
   *
   * @param node The node whose reachable set is to be retrieved.
   * @return A set of nodes reachable from the given node.
   */
  def reachableSet(node: T): Set[T] = {
    def dfs(vertex: T, visited: Set[T]): Set[T] = {
      if visited.contains(vertex) then visited
      else {
        val newVisited = visited + vertex
        neighbors(vertex).foldLeft(newVisited) {
          (acc, neighbor) => dfs(neighbor, acc)
        }
      }
    }
    dfs(node, Set.empty[T])
  }

  /**
   * Merges another graph with this graph, returning a new graph.
   *
   * @param other The other graph to merge.
   * @return A new graph with the vertices and edges of both graphs.
   */
  def merge(other: Graph[T]): Graph[T] = {
    val combinedAdjacencyList = other.adjacencyList.foldLeft(adjacencyList) {
      case (acc, (vertex, neighbors)) => {
        val updatedNeighbors = acc.getOrElse(vertex, Set.empty[T]) ++ neighbors
        acc + (vertex -> updatedNeighbors)
      }
    }
    Graph(isDirected, combinedAdjacencyList)
  }


  /**
   * Computes the Strongly Connected Components (SCCs) of the graph using Kosaraju's Algorithm.
   *
   * @return A list of sets, where each set represents a strongly connected component, ordered topologically.
   */
  def stronglyConnectedComponents: Seq[Set[T]] = {
    // Step 1: Perform DFS and push vertices to stack in the order of completion
    val visited = mutable.Set[T]()
    val stack = mutable.Stack[T]()

    def fillOrder(vertex: T): Unit = {
      if (!visited.contains(vertex)) {
        visited += vertex
        for (neighbor <- neighbors(vertex)) {
          fillOrder(neighbor)
        }
        stack.push(vertex)
      }
    }

    // Fill vertices in stack according to their finishing times
    vertices.foreach(v => if (!visited.contains(v)) fillOrder(v))

    // Step 2: Transpose the graph
    val transposedGraph = transpose()

    // Step 3: Perform DFS on transposed graph in the order defined by the stack
    visited.clear()
    val sccs = mutable.ListBuffer[Set[T]]()

    def dfsTranspose(vertex: T, component: mutable.Set[T]): Unit = {
      if (!visited.contains(vertex)) {
        visited += vertex
        component += vertex
        for (neighbor <- transposedGraph.neighbors(vertex)) {
          dfsTranspose(neighbor, component)
        }
      }
    }

    // Process all vertices in the order defined by the stack
    while (stack.nonEmpty) {
      val vertex = stack.pop()
      if (!visited.contains(vertex)) {
        val component = mutable.Set[T]()
        dfsTranspose(vertex, component)
        sccs += component.toSet
      }
    }

    // Step 4: Topologically sort the SCCs
    val sccGraph = mutable.Map[Set[T], mutable.Set[Set[T]]]()
    for (scc <- sccs) {
      sccGraph(scc) = mutable.Set[Set[T]]()
    }
    for ((vertex, neighbors) <- adjacencyList) {
      val sourceSCC = sccs.find(_.contains(vertex)).get
      for (neighbor <- neighbors) {
        val targetSCC = sccs.find(_.contains(neighbor)).get
        if (sourceSCC != targetSCC) {
          sccGraph(sourceSCC) += targetSCC
        }
      }
    }

    val topologicallySortedSCCs = mutable.ListBuffer[Set[T]]()
    val sccVisited = mutable.Set[Set[T]]()

    def topologicalSort(scc: Set[T]): Unit = {
      if (!sccVisited.contains(scc)) {
        sccVisited += scc
        for (neighborSCC <- sccGraph(scc)) {
          topologicalSort(neighborSCC)
        }
        topologicallySortedSCCs.prepend(scc)
      }
    }

    for (scc <- sccs) {
      if (!sccVisited.contains(scc)) {
        topologicalSort(scc)
      }
    }

    topologicallySortedSCCs.toSeq
  }

  /**
   * Returns the transpose of the graph.
   *
   * @return A new graph that is the transpose of this graph.
   */
  private def transpose(): Graph[T] = {
    val transposedAdjList = mutable.Map[T, Set[T]]().withDefaultValue(Set.empty[T])
    for ((vertex, neighbors) <- adjacencyList) {
      for (neighbor <- neighbors) {
        transposedAdjList(neighbor) = transposedAdjList(neighbor) + vertex
      }
    }
    Graph(isDirected = true, transposedAdjList.toMap)
  }

  /**
   * Returns a string representation of the graph.
   *
   * @return A string representing the graph.
   */
  override def toString: String = {
    adjacencyList.map {
      case (v, neighbors) => s"$v -> (${neighbors.mkString(", ")})"
    }.mkString("; ")
  }
}

object Graph {

  /**
   * Creates an empty directed graph.
   *
   * @tparam T The type of the vertices in the graph.
   * @return An empty directed graph.
   */
  def directed[T]: Graph[T] = Graph[T](isDirected = true, Map())

  /**
   * Creates an empty undirected graph.
   *
   * @tparam T The type of the vertices in the graph.
   * @return An empty undirected graph.
   */
  def undirected[T]: Graph[T] = Graph[T](isDirected = false, Map())

}
