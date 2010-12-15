package com.agical.graph

import org.scalatest.tools._
import org.scalatest._

object All {
  def main(args:Array[String]):Unit = { 
    new TraverserTest().execute
    new GraphTest().execute

    ()
  }
}

