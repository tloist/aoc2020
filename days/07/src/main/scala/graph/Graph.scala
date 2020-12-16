package graph

import com.google.common.graph.{EndpointPair, Graph, ImmutableValueGraph, ValueGraph, ValueGraphBuilder}
import customs._

import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

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

  extension (graph: Graph) {
    def predecessorsFrom(bag: BagName): LazyList[Set[BagName]] =
      lazilyCrawlGraph(bag)(_.flatMap(n => graph.predecessors(n).asScala.toSet))
    def allPredecessorsFrom(bag: BagName): Set[BagName] = predecessorsFrom(bag).foldLeft(Set.empty)(_ union _)

    def successorsFrom(bag: BagName): LazyList[Set[BagName]] =
      lazilyCrawlGraph(bag)(_.flatMap(n => graph.successors(n).asScala.toSet))
    def allSuccessorsFrom(bag: BagName): Set[BagName] = successorsFrom(bag).foldLeft(Set.empty)(_ union _)
    def bagCountToLeafs(bag: BagName): Int = {
      def bagCountToLeafsRec(bag: BagName, value: Int): Int =
        value + edgesFromOrigin(bag).toList.map(qb => bagCountToLeafsRec(qb.name, value * qb.quantity)).sum

      bagCountToLeafsRec(bag, 1) - 1 // Don't count the very first bag itself
    }

    def edgesFromOrigin(bag: BagName): Set[QuantityBag] = graph.incidentEdges(bag).asScala
      .filter(_.origin == bag)
      .map(ep => QuantityBag(ep.target, expectCountFor(ep)))
      .toSet

    def edgesToTarget(bag: BagName): Set[QuantityBag] = graph.incidentEdges(bag).asScala
      .filter(_.target == bag)
      .map(ep => QuantityBag(ep.origin, expectCountFor(ep)))
      .toSet
    
    private[this] def expectCountFor(ep: EndpointPair[BagName]): Int = graph.edgeValue(ep).toScala.getOrElse(
      throw new IllegalStateException(s"EndpointPair $ep without value?!"))

    private[this] def lazilyCrawlGraph(init: BagName)(next: Set[BagName] => Set[BagName]): LazyList[Set[BagName]] =
      LazyList.unfold(Set(init)) { known =>
        val newDiscovered = next(known)
        val diff = newDiscovered.removedAll(known)
        if diff.isEmpty then None
        else Some((diff, newDiscovered.union(known)))
      }
  }
  
  extension [T](ep: EndpointPair[T]) {
    def origin: T = ep.nodeU()
    def target: T = ep.nodeV()
  }
  
}
