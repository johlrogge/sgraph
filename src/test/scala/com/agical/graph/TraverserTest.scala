package com.agical.graph

import org.scalatest.Spec
import org.scalatest.matchers._

class TraverserTest extends Spec with MustMatchers {
  case class Knows(override val from:String, override val to:String) extends Edge[String](from, to)
  case class RelatedTo(override val from:String, override val to:String) extends Edge[String](from, to)
  
  def knows(e:Edge[String]) = e match {case n:Knows => true; case _ => false}
  def relatedTo(e:Edge[String]) = e match {case n:RelatedTo => true; case _ => false} 
  def all(e:Edge[String]) = true
    
  val empty = Set[Edge[String]]()
  
  def pending(fn:()=> Unit) = try {
    println("Not run")
  }
  
  describe("traverser") {
    val graph = new Graph[String]().connect("nd")(Knows(_,_))("ne").
                          connect("nc")(RelatedTo)("nd").
                          connect("nc")(Knows)("ne").
                          connect("nb")(RelatedTo)("ne").
                          connect("nb")(Knows)("nc").
                          connect("na")(Knows)("nb").
                          connect("na")(Knows)("nc")
    describe("breadth first") {
      it("must traverse only constrained edges") {
        Set() ++ Traverse(graph, BreadthFirst[String]).follow(knows).from(g => "na")  must equal(Set(Knows("na", "nb"), 
                                                                                                     Knows("na", "nc"), 
                                                                                                     Knows("nb", "nc"),
                                                                                                     Knows("nc", "ne")))
        Set() ++ Traverse(graph, BreadthFirst[String]).follow(relatedTo).from(g => "nb")  must equal(Set(RelatedTo("nb","ne")))
      }
      
      it("must traverse only constrained to") {
        Set() ++ Traverse(graph, BreadthFirst[String]).follow(knows).to(g => "ne")  must equal(Set(Knows("na", "nb"), 
                                                                                                     Knows("na", "nc"), 
                                                                                                     Knows("nb", "nc"),
                                                                                                     Knows("nc", "ne"),
                                                                                                     Knows("nd", "ne")))
        Set() ++ Traverse(graph, BreadthFirst[String]).follow(relatedTo).to(g => "ne")  must equal(Set(RelatedTo("nb","ne")))
      }
      
      it("must traverse incoming relationships constrained edges") {
        Set() ++ Traverse(graph, BreadthFirst[String]).follow(knows).to(g => "nc")  must equal(Set(Knows("na", "nc"),
                                                                                                   Knows("na", "nb"),
                                                                                                   Knows("nb", "nc")))
        Set() ++ Traverse(graph, BreadthFirst[String]).follow(relatedTo).to(g => "nd")  must equal(Set(RelatedTo("nc", "nd")))
      }
      
      it ("must traverse circular structures"){
        val circular = graph.connect("a")(Knows)("b").
                             connect("b")(Knows)("c").
                             connect("c")(Knows)("a")
        Set() ++ Traverse(circular, BreadthFirst[String]).follow(all).from(g => "a") must equal (Set(Knows("a", "b"),
                                                                                                  Knows("b", "c"), 
                                                                                                  Knows("c", "a")))
      }
    }
  }
}
