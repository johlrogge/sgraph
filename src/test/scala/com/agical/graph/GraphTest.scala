package com.agical.graph

import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers

class GraphTest extends Spec with MustMatchers {
  val graph = new Graph[String]()
  val empty = Set[Edge[String]]()
  case class KindOf[N](override val from:N, override val to:N) extends Edge[N](from, to)
  
  describe("graph") {
    describe("- node creating and searching") {
      
      it("must find no nodes when none exists") {
        Find.nodesIn(graph).matching(EqualTo("name","some/node")).toList must equal(List())
      }  
        
      it("must find no nodes with no constraint") {
        val updatedGraph = graph.create("This is the node")
        updatedGraph.find().toList must equal(List())
      }
            
      it("must find node by attribute") {
        val updatedGraph = graph.create("This is the node", "name" -> "some/node")
        Find.nodesIn(updatedGraph).matching(EqualTo("name","some/node")).toList must equal(List("This is the node"))
      }
       
      it("must find node by different values") {
        val updatedGraph = graph.create("firstNode", "name" -> "first").create("secondNode", "name" -> "second")
        Find.nodesIn(updatedGraph).matching(EqualTo("name","first")).toList must equal (List("firstNode"))
        Find.nodesIn(updatedGraph).matching(EqualTo("name","second")).toList must equal (List("secondNode"))
      }
      
      it("must find node by attribute containing") {
        val updatedGraph = graph.create("firstNode", "name" -> Set("first", "uno")).
                                 create("secondNode", "name" -> Set("second", "due"))
        Find.nodesIn(updatedGraph).matching(Contains("name", "uno")).toList must equal (List("firstNode"))
        Find.nodesIn(updatedGraph).matching(Contains("name", "due")).toList must equal (List("secondNode"))
      }
      
      it("must find node by multiple attribtes") {
        val updatedGraph = graph.create("a multiNode", "name" -> "some/node", "time" -> 4711L)
        Find.nodesIn(updatedGraph).matching(EqualTo("name", "some/node") && 
                                            EqualTo("time", 4711L)).toList must equal(List("a multiNode"))
      }
        
      it("must not find node by multiple attribtes if one is wrong") {
        val updatedGraph = graph.create("a multiNode", "name" -> "WRONG!", "time" -> 4711L)
        Find.nodesIn(updatedGraph).matching(EqualTo("name","some/node") && 
                                            EqualTo("time", 4711L)).toList must equal(List())
      }
      
      it("must be able to return a calculated result per node") {
        val updatedGraph = graph.create("54321").create("1234567890")
        Find.nodesIn(updatedGraph).matching(Nodes(n => Some(n.length))) must equal(Set(5, 10))
      }
    }
    
    
    
    describe ("- connecting and traversing") {
      def kindOf[N](edge:Edge[N]) = edge match {
        case KindOf(_, _) => true
        case _ => false
      }
      
      it("must not find any relatives if no relatives exist") {
        val updatedGraph = graph.create("A rectangle", "shape" -> "rectangle").
                                 create("A square", "shape" -> "square")
        
        val res:List[Edge[String]] = Traverse[String](updatedGraph, BreadthFirst(_)).follow(kindOf).from(g => "A rectangle").toList 
        res must be(List())
      }
      
      it("must find relatives by edge-type") {
        val nodeGraph = graph.create("A rectangle", "shape" -> "rectangle").
                              create("A square", "shape" -> "square")
        val updatedGraph = nodeGraph.connect("A square")(KindOf(_,_))("A rectangle")
       
        Traverse(updatedGraph, BreadthFirst[String]).follow(kindOf).from(g => "A square").toList must be(List(KindOf("A square", "A rectangle")))
        
      }
      
      it("must maintain connections when overwriting node") {
        val nodeGraph = graph.create("A rectangle", "shape" -> "rectangle").
                              create("A square", "shape" -> "square")
                              
        val updatedGraph = nodeGraph.connect("A square")(KindOf(_,_))("A rectangle").
                                     create("A square", "shape" -> "square")
       
        Traverse(updatedGraph, BreadthFirst[String]).follow(kindOf).from(g => "A square").toList must be(List(KindOf("A square", "A rectangle")))

      }
    }
  }
}
