package com.agical.graph


abstract case class Expression[N,R]() {
  protected val EMPTY_SET = Set[R]()
  def apply(nodes:Set[N], source:Map[String, Map[Any, Set[N]]]):Set[R]
  
}

abstract case class KeyExpression[N](key:String) extends Expression[N, N] {
  def apply(nodes:Set[N], source:Map[String, Map[Any, Set[N]]]):Set[N] = {
    source.get(key) match {
      case Some(idx) => matchKey(idx)
      case None => EMPTY_SET
    }
    
  }
  def &&(other:KeyExpression[N]) = And(this, other)
  def matchKey(value:Map[Any, Set[N]]):Set[N]
}

case class And[N](left:Expression[N, N], right:Expression[N, N]) extends Expression[N, N] {
  def apply(nodes:Set[N], source:Map[String, Map[Any, Set[N]]]):Set[N] = {
    left(nodes, source) ** right(nodes, source)
  }
}

case class Nodes[N, R](matchNodes:N => Iterable[R]) extends Expression[N, R] {
  def apply(nodes:Set[N], source:Map[String, Map[Any, Set[N]]]):Set[R] = {
    nodes.flatMap(matchNodes)
  }
}

case class AllNodes[N]() extends Nodes[N, N](n => Some(n))

case class EqualTo[N](override val key:String, left:Any) extends KeyExpression[N](key) {
  def matchKey(value:Map[Any, Set[N]]):Set[N] = {
    value.get(left) match {
      case Some(nodes) => nodes
      case None => EMPTY_SET
    }
  }
}

case class Contains[N, K](override val key:String, left:K) extends KeyExpression[N](key) {
  def matchKey(value:Map[Any, Set[N]]):Set[N] = {
    (EMPTY_SET /: value)((r, kv) => kv match {
      case (k:Set[K], value:Set[N]) => if(k.contains(left)) r ++ kv._2 else r 
      case _ => r
    })
  }
}


object Find {
  def nodesIn[N](graph:Graph[N]) = {
    new {
      def matching[R](expr:Expression[N,R]) = graph.find(expr)
    }
  }
}
