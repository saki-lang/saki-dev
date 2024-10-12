package saki.util

/**
 * A class representing a graph data structure.
 *
 * @param adjacencyList The adjacency list representing the graph.
 * @param isDirected    A boolean indicating whether the graph is directed.
 * @tparam T The type of the vertices in the graph.
 */
class Graph[T](
  val isDirected: Boolean = true,
  private val adjacencyList: Map[T, List[T]] = Map(),
) {

  /**
   * Adds a vertex to the graph, returning a new graph.
   *
   * @param vertex The vertex to add.
   * @return A new graph with the added vertex.
   */
  def addVertex(vertex: T): Graph[T] = {
    if adjacencyList.contains(vertex) then this else {
      Graph(isDirected, adjacencyList + (vertex -> List()))
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
    val updatedAdjList = updatedGraph.adjacencyList + (v1 -> (v2 :: updatedGraph.adjacencyList(v1)))
    val finalAdjList = if isDirected then updatedAdjList else {
      updatedAdjList + (v2 -> (v1 :: updatedAdjList.getOrElse(v2, List())))
    }
    Graph(isDirected, finalAdjList)
  }

  /**
   * Retrieves the neighbors of a given vertex.
   *
   * @param vertex The vertex whose neighbors are to be retrieved.
   * @return A list of neighbors of the given vertex.
   */
  def neighbors(vertex: T): List[T] = adjacencyList.getOrElse(vertex, List())

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
   * Merges another graph with this graph, returning a new graph.
   *
   * @param other The other graph to merge.
   * @return A new graph with the vertices and edges of both graphs.
   */
  def merge(other: Graph[T]): Graph[T] = {
    val combinedAdjacencyList = other.adjacencyList.foldLeft(adjacencyList) {
      case (acc, (vertex, neighbors)) => {
        val updatedNeighbors = acc.getOrElse(vertex, List()) ++ neighbors
        acc + (vertex -> updatedNeighbors.distinct) // Ensure no duplicate neighbors
      }
    }
    Graph(isDirected, combinedAdjacencyList)
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
