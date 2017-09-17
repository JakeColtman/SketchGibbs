package SumProduct

import org.scalatest.{FlatSpec, Matchers}

trait Clique {
  def separator_variables_from(iClique: Clique): Clique
  def combine_with(iClique: Clique): Clique
  def vertices: List[Vertex]
}

case class ListClique(vertices: List[Vertex]) extends Clique {
  def separator_variables_from(clique: Clique): Clique = {
    ListClique(clique.vertices.toSet.intersect(vertices.toSet).toList)
  }

  def combine_with(clique: Clique) : Clique = {
    ListClique(clique.vertices.toSet.union(vertices.toSet).toList)
  }
}

case object CliqueCombiner {

  def is_vertex_addable_to_clique(vertex: Vertex, clique: Clique) : Boolean = {
    !clique.vertices.exists(v => !v.outgoing_edges.exists(e => e.to == vertex)) //TODO don't assume bidirectionality
  }

  def can_cliques_be_combined(cliquea: Clique, cliqueb: Clique) : Boolean = {
    val cannot_a_to_b : List[Boolean] = for (v <- cliquea.vertices) yield {!is_vertex_addable_to_clique(v, cliqueb)}
    val cannot_b_to_a : List[Boolean] = for (v <- cliqueb.vertices) yield {!is_vertex_addable_to_clique(v, cliquea)}
    cannot_a_to_b.exists(x => x) || cannot_b_to_a.exists(x => x)
  }
}

case object CliqueFactory {

  def apply(vertices: List[Vertex]) : Clique = {
    ListClique(vertices)
  }

  def apply(cliqueA: Clique, cliqueB: Clique): Clique = {
    ListClique(cliqueA.vertices ++ cliqueB.vertices)
  }

  def apply(vertex: Vertex) : List[Clique] = {
    if (vertex.outgoing_edges.isEmpty) return List(CliqueFactory(List(vertex)))
    var cliques = vertex.outgoing_edges.map(x => CliqueFactory(List(vertex, x.to)))

    def try_combine_cliques(cliques: List[Clique]) : (Boolean, List[Clique]) = {
      var new_cliques = cliques
      for(clique_a<-new_cliques;clique_b<-new_cliques) yield {
        if (CliqueCombiner.can_cliques_be_combined(clique_a, clique_b)) {
          new_cliques = new_cliques.filter(x => (x!= clique_a) & (x!= clique_b))
          new_cliques = new_cliques ++ List(clique_a.combine_with(clique_b))
        }
        else clique_a
      }
      if (new_cliques.size != cliques.size) (false, new_cliques)
      else (true, new_cliques)
    }

    var done = false
    while (!done) {
      val results = try_combine_cliques(cliques)
      done = results._1
      cliques= results._2
    }

    cliques

  }

}

class CliqueSpec extends FlatSpec with Matchers {

    "A clique factory " should " return a degenerate set of cliques for variables not connected to anything else" in {
      val a = VertexFactory("a")

      CliqueFactory(a).head should be (CliqueFactory(List(a)))
    }

}

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
