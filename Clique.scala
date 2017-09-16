package SumProduct

trait IClique {
  def separator_from(iClique: IClique): IClique
  def combine_with(iClique: IClique): IClique
  def vertices: List[Vertex]
}

case class Clique(vertices: List[Vertex]) extends IClique {
  def separator_from(clique: IClique): IClique = {
    Clique(clique.vertices.toSet.intersect(vertices.toSet).toList)
  }

  def combine_with(clique: IClique) : IClique = {
    Clique(clique.vertices.toSet.union(vertices.toSet).toList)
  }
}

