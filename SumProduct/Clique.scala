package SumProduct

import org.scalatest.{FlatSpec, Matchers}

trait Clique[NodeType] {
  def separator_variables_from(iClique: Clique[NodeType]): Clique[NodeType]
  def vertices: Set[Vertex[NodeType]]
}
//
//case class ListClique(vertices: Set[Vertex]) extends Clique {
//  def separator_variables_from(clique: Clique): Clique = {
//    ListClique(clique.vertices.intersect(vertices))
//  }
//}
//
//case object CliqueCombiner {
//
//  def is_vertex_addable_to_clique(vertex: Vertex, clique: Clique) : Boolean = {
//    !clique.vertices.filter(v => v != vertex).exists(v => !v.outgoing_edges.exists(e => e.to == vertex)) //TODO don't assume bidirectionality
//  }
//
//  def can_cliques_be_combined(cliquea: Clique, cliqueb: Clique) : Boolean = {
//    val cannot_a_to_b : Set[Boolean] = for (v <- cliquea.vertices) yield {!is_vertex_addable_to_clique(v, cliqueb)}
//    val cannot_b_to_a : Set[Boolean] = for (v <- cliqueb.vertices) yield {!is_vertex_addable_to_clique(v, cliquea)}
//    !(cannot_a_to_b.exists(x => x) || cannot_b_to_a.exists(x => x))
//  }
//}
//
//case object CliqueFactory {
//
//  def apply(vertices: Set[Vertex]) : Clique = {
//    ListClique(vertices)
//  }
//
//  def apply(cliqueA: Clique, cliqueB: Clique): Clique = ListClique(cliqueA.vertices.union(cliqueB.vertices))
//
//  def apply(vertex: Vertex) : List[Clique] = {
//    if (vertex.outgoing_edges.isEmpty) {
//      return List(CliqueFactory(Set(vertex)))
//    }
//    var cliques = vertex.outgoing_edges.map(x => CliqueFactory(Set(vertex, x.to)))
//    def try_combine_cliques(cliques: List[Clique]) : (Boolean, List[Clique]) = {
//      var new_cliques = cliques
//
//      for(clique_a<-new_cliques;clique_b<-new_cliques) yield {
//        if (CliqueCombiner.can_cliques_be_combined(clique_a, clique_b)) {
//          new_cliques = new_cliques.filter(x => (x!= clique_a) & (x!= clique_b))
//          new_cliques = new_cliques ++ List(CliqueFactory(clique_a, clique_b))
//        }
//        else clique_a
//      }
//      if (new_cliques.size != cliques.size) (false, new_cliques)
//      else (true, new_cliques)
//    }
//
//    var done = false
//    while (!done) {
//      val results = try_combine_cliques(cliques)
//      done = results._1
//      cliques= results._2
//    }
//    cliques
//  }
//}
//
//class CliqueSpec extends FlatSpec with Matchers {
//
//  "A clique combiner " should " be willing to add a vertex that connects to all members of a clique " in {
//    val a = VertexFactory("a")
//    val c = VertexFactory("c")
//    val b = VertexFactory("b")
//    GraphFactory(List(a, b, c)).add_edges((a<->b) ++ (b<->c) ++ (a<->c))
//    val starting_clique = CliqueFactory(Set(a,b))
//    CliqueCombiner.is_vertex_addable_to_clique(c, starting_clique) should be (true)
//  }
//
//  "A clique combiner " should " be not willing to add a vertex that doesn't connect to all members of a clique " in {
//    val a = VertexFactory("a")
//    val c = VertexFactory("c")
//    val b = VertexFactory("b")
//    GraphFactory(List(a, b, c)).add_edges((a<->b) ++ (b<->c))
//    val starting_clique = CliqueFactory(Set(a,b))
//    CliqueCombiner.is_vertex_addable_to_clique(c, starting_clique) should be (false)
//  }
//
//  "A clique combiner " should " be willing to combine cliques if all members of both cliques can be added to the other " in {
//    val a = VertexFactory("a")
//    val c = VertexFactory("c")
//    val b = VertexFactory("b")
//    val d = VertexFactory("d")
//
//    GraphFactory(List(a, b, c, d)).add_edges((a<->b) ++ (c<->d) ++ (b<->c) ++ (b<->d) ++ (a<->c) ++ (a<->d))
//    val cliqueA = CliqueFactory(Set(a, b))
//    val cliqueB = CliqueFactory(Set(c, d))
//    CliqueCombiner.can_cliques_be_combined(cliqueA, cliqueB) should be (true)
//  }
//
//  "A clique combiner " should " not be willing to combine cliques if any member of one clique can't be added to the other " in {
//    val a = VertexFactory("a")
//    val c = VertexFactory("c")
//    val b = VertexFactory("b")
//    val d = VertexFactory("d")
//
//    GraphFactory(List(a, b, c, d)).add_edges((a<->b) ++ (c<->d) ++ (b<->d) ++ (b<->c))
//    val cliqueA = CliqueFactory(Set(a, b))
//    val cliqueB = CliqueFactory(Set(c, d))
//    CliqueCombiner.can_cliques_be_combined(cliqueA, cliqueB) should be (false)
//
//  }
//
//  "A clique factory " should " return a degenerate set of cliques for variables not connected to anything else" in {
//    val a = VertexFactory("a")
//
//    CliqueFactory(a).head should be (CliqueFactory(Set(a)))
//  }
//
//  "A clique factory " should " match pairs into cliques" in {
//    val a = VertexFactory("a")
//    val c = VertexFactory("c")
//    val b = VertexFactory("b")
//
//    GraphFactory(List(a, b, c)).add_edges((a<->b) ++ (b<->c))
//    CliqueFactory(a) should contain theSameElementsAs List(CliqueFactory(Set(a, b)))
//    CliqueFactory(b) should contain theSameElementsAs List(CliqueFactory(Set(b,a)), CliqueFactory(Set(b,c)))
//    CliqueFactory(c) should contain theSameElementsAs List(CliqueFactory(Set(c,b)))
//  }
//
//  "A clique combiner " should " be able to combine cliques with shared elements " in {
//    val a = VertexFactory("a")
//    val c = VertexFactory("c")
//    val b = VertexFactory("b")
//
//    GraphFactory(List(a, b, c)).add_edges((a<->b) ++ (b<->c) ++ (a<->c))
//
//    CliqueCombiner.can_cliques_be_combined(CliqueFactory(Set(a, b)), CliqueFactory(Set(a, c))) should be (true)
//  }
//
//  "A clique factory " should " not duplicate entries when combining cliques with overlapping elements " in {
//    val a = VertexFactory("a")
//    val c = VertexFactory("c")
//    val b = VertexFactory("b")
//
//    GraphFactory(List(a, b, c)).add_edges((a<->b) ++ (b<->c) ++ (a<->c))
//
//    CliqueFactory(CliqueFactory(Set(a, b)), CliqueFactory(Set(a, c))).vertices.size should be (3)
//  }
//
//  "Cliques " should " base equality of set of vertices contained " in {
//    val a = VertexFactory("a")
//    val c = VertexFactory("c")
//    CliqueFactory(Set(a, c)) should be (CliqueFactory(Set(a, c)))
//    CliqueFactory(Set(a, c)) should be (CliqueFactory(Set(c, a)))
//  }
//
//  "A clique factory " should " be able to combine into bigger cliques" in {
//    val a = VertexFactory("a")
//    val c = VertexFactory("c")
//    val b = VertexFactory("b")
//
//    GraphFactory(List(a, b, c)).add_edges((a<->b) ++ (b<->c) ++ (a<->c))
//    CliqueFactory(a).head.vertices should contain theSameElementsAs Set(a, b, c)
//    CliqueFactory(a).head.vertices should contain theSameElementsAs CliqueFactory(b).head.vertices
//    CliqueFactory(a).size should be (1)
//  }
//
//
//}
//
////
////  "A graph " should " be able to identify a simple clique" in {
////    val a = CliquePotential("a")
////    val b = Separator("b")
////    val c = Separator("c")
////    val d = Separator("d")
////
////    val edgeAB = UndirectedEdge(a, b)
////    val edgeBC = UndirectedEdge(b, c)
////    val edgeAC = UndirectedEdge(a, c)
////    val edgeAD = UndirectedEdge(a, d)
////
////    val graph = Graph(List(a, b, c, d), List(edgeAB, edgeBC, edgeAC, edgeAD))
////    graph.can_vertex_add_to_clique(Clique(List(a,c)), b) should be (true)
////    graph.can_add_clique_to_clique(Clique(List(a,b)), Clique(List(a, c))) should be (true)
////    //
////    graph.find_cliques_from(a).head.vertices should contain theSameElementsAs List(a, b, c)
////    graph.find_cliques_from(c).head.vertices should contain theSameElementsAs List(a, b, c)
////    graph.find_cliques_from(d).head.vertices should contain theSameElementsAs List(a, d)
////    graph.find_cliques_from(a).tail.head.vertices should contain theSameElementsAs List(a, d)
////    //graph.find_cliques_from(c) should contain theSameElementsAs (List(a, b, c))
////  }
////
////  "A graph " should " be easy be convertable into a clique graph" in {
////    val a = CliquePotential("a")
////    val b = Separator("b")
////    val c = Separator("c")
////    val d = Separator("d")
////
////    val edgeAB = UndirectedEdge(a, b)
////    val edgeBC = UndirectedEdge(b, c)
////    val edgeAC = UndirectedEdge(a, c)
////    val edgeAD = UndirectedEdge(a, d)
////
////    val graph = Graph(List(a, b, c, d), List(edgeAB, edgeBC, edgeAC, edgeAD))
////    graph.convert_to_list_of_cliques()
////  }
