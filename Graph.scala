package SumProduct

import org.scalatest.{FlatSpec, Matchers}

trait IGraph {
  val vertices: List[Vertex]
  val edges: List[Edge]
  def contains(vertex: Vertex): Boolean
  def contains(edge: Edge): Boolean

}

//case class Graph(vertices: List[Vertex], var edges: List[Edge]) {
//  def vertex_iterator: List[Vertex] = vertices
//
//  def remove_edge(edge: Edge) : Unit = {
//    edges = edges.filter(x => edge != x)
//  }
//
//  def edge_exists(edge: Edge): Boolean = {
//    edge match {
//      case DirectedEdge(from_node, to_node) => edges.flatMap(x => x.to_directed_edges()).contains(edge)
//      case UndirectedEdge(first_node, second_node) => edges.contains(edge) | edges.contains(UndirectedEdge(second_node, first_node))
//      case _ => false
//    }
//
//  }
//
//  def neighbours_of(vertex: Vertex) : List[Vertex] = {
//    edges.flatMap(edge => edge.to_directed_edges()).filter(edge => edge.from_node == vertex).map(edge => edge.to_node)
//  }
//
//  def find_a_cliquo_from(vertex: Vertex): List[Vertex] = {
//    val neighbours = neighbours_of(vertex)
//
//    val cliquos = for (n0 <- neighbours; n1 <- neighbours) yield {
//      println(n0 + " to " + n1)
//      if (edge_exists(UndirectedEdge(n0, n1))) {
//        println("Found")
//        Some(List(vertex, n0, n1))
//      }
//      else None
//    }
//    if (neighbours.isEmpty) List(vertex)
//    else cliquos.filter(x => x.isDefined).map(x => x.get).head
//  }
//
//  def find_cliques_from(vertex: Vertex): List[Clique] = {
//    val neighbors = neighbours_of(vertex)
//    if (neighbors.isEmpty) return List(Clique(List(vertex)))
//
//    var biggest_cliques = neighbors.map(x => Clique(List(vertex, x)))
//
//    def try_combine_cliques(cliques: List[Clique]) : (Boolean, List[Clique]) = {
//      var new_cliques = cliques
//      for(clique_a<-new_cliques;clique_b<-new_cliques) yield {
//        if (can_add_clique_to_clique(clique_a, clique_b)) {
//          new_cliques = new_cliques.filter(x => (x!= clique_a) & (x!= clique_b))
//          new_cliques = new_cliques ++ List(clique_a.combine_with(clique_b))
//        }
//        else clique_a
//      }
//      if (new_cliques.size != cliques.size) (false, new_cliques)
//      else (true, new_cliques)
//    }
//
//    var done = false
//    while (!done) {
//      val results = try_combine_cliques(biggest_cliques)
//      done = results._1
//      biggest_cliques= results._2
//    }
//
//    biggest_cliques
//  }
//
//  def can_vertex_add_to_clique(clique: Clique, vertex: Vertex) = {
//    !clique.vertices.exists(c => !edge_exists(UndirectedEdge(vertex, c)))
//  }
//
//  def can_add_clique_to_clique(clique_a: Clique, clique_b: Clique): Boolean ={
//    val seps = clique_a.separator_from(clique_b)
//    val atob = !clique_a.vertices.filter(x => !seps.vertices.contains(x)).map(x => can_vertex_add_to_clique(clique_b, x)).exists(x => !x)
//    val btoa = !clique_b.vertices.filter(x => !seps.vertices.contains(x)).map(x => can_vertex_add_to_clique(clique_a, x)).exists(x => !x)
//    atob & btoa
//  }
//
//  def convert_to_markov_graph() : Graph = {
//   Graph(vertices, edges.map(edge => edge match {
//     case DirectedEdge(from_node, to_node) => UndirectedEdge(from_node, to_node)
//     case UndirectedEdge(first_node, second_node) => edge
//   }))
//  }
//
//  def convert_to_list_of_cliques() : List[Clique] = {
//    vertices.flatMap(x => find_cliques_from(x))
//  }
//}

class GraphSpec extends FlatSpec with Matchers {
//  "A graph " should "return a list of its vertices" in {
//    val a = CliquePotential("a")
//    val b = Separator("a")
//
//    val graph = Graph(List(a, b), List())
//    graph.vertex_iterator should be (List(a, b))
//  }
//
//  "A graph " should "know if an undirected edge is in edge list" in {
//    val a = CliquePotential("a")
//    val b = Separator("a")
//    val c = Separator("c")
//
//    val edge = UndirectedEdge(a, b)
//    val non_existant_edge = UndirectedEdge(a, c)
//    val graph = Graph(List(a, b), List(edge))
//    graph.edge_exists(edge) should be (true)
//    graph.edge_exists(non_existant_edge) should be (false)
//  }
//
//  "A graph " should "be able to intelligently compare directed and undirected edges" in {
//    val a = CliquePotential("a")
//    val b = Separator("a")
//
//    val edge = UndirectedEdge(a, b)
//    val search_edge = DirectedEdge(a, b)
//    val graph = Graph(List(a, b), List(edge))
//    graph.edge_exists(search_edge) should be (true)
//  }
//
//  "A graph " should " remove edges on request" in {
//    val a = CliquePotential("a")
//    val b = Separator("a")
//
//    val edge = UndirectedEdge(a, b)
//    val graph = Graph(List(a, b), List(edge))
//    graph.remove_edge(edge)
//    graph.edge_exists(edge) should be (false)
//  }
//
//
//  "A graph " should " know the neighbours of a vertex" in {
//    val a = CliquePotential("a")
//    val b = Separator("b")
//    val c = Separator("c")
//
//    val edgeAB = UndirectedEdge(a, b)
//    val edgeBC = DirectedEdge(b, c)
//
//    val graph = Graph(List(a, b, c), List(edgeAB, edgeBC))
//    graph.neighbours_of(a) should be (List(b))
//    graph.neighbours_of(b) should be (List(a, c))
//    graph.neighbours_of(c) should be (List())
//  }
//
//  "A graph " should " should return a degenerate clique for variables in clique by themselves" in {
//    val a = CliquePotential("a")
//    val b = Separator("b")
//    val c = Separator("c")
//
//    val edgeAB = UndirectedEdge(a, b)
//    val edgeBC = DirectedEdge(b, c)
//
//    val graph = Graph(List(a, b, c), List(edgeAB, edgeBC))
//    graph.find_a_cliquo_from(c) should be (List(c))
//  }
//
//  "A graph " should " be able to identify a simple clique" in {
//    val a = CliquePotential("a")
//    val b = Separator("b")
//    val c = Separator("c")
//    val d = Separator("d")
//
//    val edgeAB = UndirectedEdge(a, b)
//    val edgeBC = UndirectedEdge(b, c)
//    val edgeAC = UndirectedEdge(a, c)
//    val edgeAD = UndirectedEdge(a, d)
//
//    val graph = Graph(List(a, b, c, d), List(edgeAB, edgeBC, edgeAC, edgeAD))
//    graph.can_vertex_add_to_clique(Clique(List(a,c)), b) should be (true)
//    graph.can_add_clique_to_clique(Clique(List(a,b)), Clique(List(a, c))) should be (true)
//    //
//    graph.find_cliques_from(a).head.vertices should contain theSameElementsAs List(a, b, c)
//    graph.find_cliques_from(c).head.vertices should contain theSameElementsAs List(a, b, c)
//    graph.find_cliques_from(d).head.vertices should contain theSameElementsAs List(a, d)
//    graph.find_cliques_from(a).tail.head.vertices should contain theSameElementsAs List(a, d)
//    //graph.find_cliques_from(c) should contain theSameElementsAs (List(a, b, c))
//  }
//
//  "A graph " should " be easy be convertable into a clique graph" in {
//    val a = CliquePotential("a")
//    val b = Separator("b")
//    val c = Separator("c")
//    val d = Separator("d")
//
//    val edgeAB = UndirectedEdge(a, b)
//    val edgeBC = UndirectedEdge(b, c)
//    val edgeAC = UndirectedEdge(a, c)
//    val edgeAD = UndirectedEdge(a, d)
//
//    val graph = Graph(List(a, b, c, d), List(edgeAB, edgeBC, edgeAC, edgeAD))
//    graph.convert_to_list_of_cliques()
//  }

}