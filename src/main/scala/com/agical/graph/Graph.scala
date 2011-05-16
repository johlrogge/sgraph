package com.agical.graph

abstract case class Direction[N]() {
  def edges(g:Graph[N], from:N):Iterable[Edge[N]]
  def nextNode(e:Edge[N]):N
}

case class Outgoing[N]() extends Direction[N]() {
  def edges(g:Graph[N], from:N) = {
    g.outgoingEdges(from)
  }
  def nextNode(e:Edge[N]) = e.to
}

case class Incoming[N]() extends Direction[N]() {
  def edges(g:Graph[N], to:N) = {
    g.incomingEdges(to)
  }
  
  def nextNode(e:Edge[N]) = e.from
}

case class Traverse[N](g: Graph[N], traverser: (Edge[N] => Boolean) => Traverser[N])(implicit val m: Manifest[N]) {
  def follow(constraint:(Edge[N]) => Boolean) = new {
     def from(start:Graph[N] => N):Iterable[Edge[N]] = traverser(constraint).traverse(g, start(g), Outgoing())
     def to(start:Graph[N] => N):Iterable[Edge[N]] = traverser(constraint).traverse(g, start(g), Incoming())
  }
}
 
abstract case class Traverser[N](val constraint:Edge[N] => Boolean) {
   def traverse(graph: Graph[N], n: N, dir: Direction[N]): Iterable[Edge[N]]
}
 
case class BreadthFirst[N](override val constraint:Edge[N] => Boolean) extends Traverser[N](constraint) {
  def traverse(graph:Graph[N], n:N, dir:Direction[N]) = new Iterable[Edge[N]]() {
    var visited = Set[Edge[N]]()
    override def iterator = new Iterator[Edge[N]] {
      visited = Set[Edge[N]]()
      var filtered =  dir.edges(graph,n).filter(constraint)
      var edges = filtered.elements
      def next:Edge[N] = {
        val e = edges.next
        visited = visited +e
        e
      }
      
      def hasNext = edges.hasNext || hasNextLevel
      private def hasNextLevel = {
        filtered = filtered.flatMap(e => dir.edges(graph, dir.nextNode(e))).filter(constraint).filter(!visited.contains(_))
        edges = filtered.elements
        edges.hasNext
      }
    }
  }
}

class Edge[N](val from:N, val to:N) {
  def apply[N](from:N, to: N) = new Edge[N](from, to)
  def sameEdge(from: N, to: N) = new Edge(from, to)
}

object Edge {
  def unapply[N](e: Edge[N]) = Some((e.from, e.to))
}

class Graph[N](private val indexes:Map[String, Map[Any, Set[N]]], 
               private val attributes:Map[N, Map[String, Any]],
               private val outgoingEdges:Map[N, Set[Edge[N]]],
               private val incomingEdges:Map[N, Set[Edge[N]]],
               private val nodes:Set[N]) {
  def this() = this(Map(), Map(), Map(), Map(), Set())
  
  def getNodes = nodes
  
  /**
   finds all nodes mathing <em>all</em> the attributes given
   */
  def find[R](attributes:Expression[N,R]*):Iterable[R] = find(attributes.toList)
  
  def find[R](attributes:List[Expression[N,R]]):Iterable[R] = attributes match {
    case head :: rest => (doFind(head) /: rest)((acc, e) => if (acc.isEmpty) acc  else acc ** doFind(e))
    case Nil => Set[R]()
  }

  /**
   creates a node with the given attributes
   */
  def create(data:N, attributes:(String, Any)*) = {
    val atts = Map() ++ attributes
    add(data, Some(atts))
  }
  
  
  private def remove(node:N) = {
    val attrs = attributes.get(node) 
    (for (attributes <- attrs) yield {
        (this /: attributes)((res, e) => {
          res.indexes.get(e._1) match {
            case Some(idx) => idx.get(e._2) match {
              case Some(nds) => new Graph(res.indexes + (e._1 -> (idx + (e._2 -> (nds - node)))), this.attributes - node, outgoingEdges - node, incomingEdges - node, nodes - node)
              case None => res
            } 
            case None => res
          } 
        })
    }).getOrElse(this)
  }
  
  private def add(node:N, attrs:Option[Map[String, Any]]):Graph[N] = {
    attrs.map(attributes => {
        val newAttributes = this.attributes.updated(node, attributes)
        val newIndexes = (indexes /: attributes)((res, e) => {
          res.get(e._1) match {
            case Some(idx) => idx.get(e._2) match {
              case Some(nodes) => (res + (e._1 -> (idx + (e._2 -> (nodes + node)))))
              case None => res + (e._1 -> (idx + (e._2 -> Set(node))))
            } 
            case None => res + (e._1 -> Map(e._2 -> Set(node)))
          } 
          
        })
        new Graph(newIndexes, newAttributes, outgoingEdges, incomingEdges, nodes + node)
      }).getOrElse(new Graph(indexes, attributes, outgoingEdges, incomingEdges, nodes + node))
  }
  
  private def replaceNode(node:N, attributes:Option[Map[String, Any]]):Graph[N] = {
    val removed = remove(node)
    removed.add(node, attributes)
  }
  
  def connect(from:N)(fn:(N, N) => Edge[N])(to:N) = {
      new Graph(indexes, attributes, 
                updateIndex(from, fn(from, to), outgoingEdges), 
                updateIndex(to, fn(from, to), incomingEdges),
                nodes + from + to)
  }
  
  private def updateIndex(key:N, edge:Edge[N], index:Map[N, Set[Edge[N]]]) = {
      val edgesForNode = index.getOrElse(key, Set())
      val removedEdges = index - key
      removedEdges.updated(key, edgesForNode + edge)
  }
  
  private def doFind[R](expression:Expression[N,R]):Set[R] = { 
    expression(nodes, indexes)
  }
  
  def outgoingEdges(fromNode:N):Iterable[Edge[N]] = outgoingEdges.getOrElse(fromNode, Set())
  def incomingEdges(fromNode:N):Iterable[Edge[N]] = incomingEdges.getOrElse(fromNode, Set())
  
  override def equals(o:Any) = {
    o match {
      case other:Graph[_] => {
	    other.incomingEdges == incomingEdges &&
	    other.outgoingEdges == outgoingEdges &&
	    other.nodes == nodes &&
	    other.attributes == attributes        
      }
      case _ => false
    }
  }
  
  override def hashCode = nodes.hashCode()
  
  override def toString() = attributes.mkString("attributes: {", ", ", "}\n") +
                            outgoingEdges.mkString("edges: {", ", ", "}")
  
  def edges = (Set[Edge[N]]() /: outgoingEdges)((r, es) => (r ++ es._2))
}
