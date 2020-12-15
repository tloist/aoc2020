package graph

import com.google.common.graph.{Graph, ImmutableValueGraph, ValueGraph, ValueGraphBuilder}
import customs._
import scala.jdk.CollectionConverters._

opaque type Graph = ImmutableValueGraph[BagName, Int]

object Graph {
  def apply(rules: Iterable[Rule]): Graph = {
    val builder = ValueGraphBuilder.directed().immutable().asInstanceOf[ImmutableValueGraph.Builder[BagName, Int]];
    for {
      rule <- rules
      target <- rule.contains
    } { builder.putEdgeValue(rule.bag, target.name, target.quantity) }
    builder.build();
  }

  extension (graph: Graph)
    def predecessorsFrom(bag: BagName): LazyList[Set[BagName]] = LazyList.unfold(Set(bag)) { known =>
      val newDiscovered = known.flatMap(n => graph.predecessors(n).asScala.toSet)
      val diff = newDiscovered.removedAll(known)
      if diff.isEmpty then None
      else Some((diff, newDiscovered.union(known)))
    }

    def allPredecessorsFrom(bag: BagName): Set[BagName] = predecessorsFrom(bag).foldLeft(Set.empty)(_ union _)
}
