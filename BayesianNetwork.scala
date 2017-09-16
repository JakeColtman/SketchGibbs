package SumProduct


//case class BayesianNetwork(nodes: List[Node]) {
//  def is_finished(): Boolean = !nodes.exists(x => !x.is_finished())
//  def run_iteration() = nodes.foreach(node => node.neighbours.keys.foreach(neigh => if (node.can_send_to(neigh)) neigh.receive_message(node, node.message_to(neigh))))
//  def run_to_completion() = {
//    while (!is_finished()){
//      run_iteration()
//    }
//  }
//}
