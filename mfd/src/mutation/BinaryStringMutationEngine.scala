package mutation

import genotype.{BinaryStringGenotype, Genotype}
import util.Random

class BinaryStringMutationEngine(prob: Double) extends MutationEngine {
  var mProb = prob

  def mutate(g0 : Genotype){
//    if(g.getClass != ){
//      println("BinaryStringMutationEngine only takes BinaryStringGenotype")
//      System.exit(-1)
//    }
    var g = g0.asInstanceOf[BinaryStringGenotype]

    var rand = new Random()

    for (i <- 0 until g.length){
      if (rand.nextDouble() < mProb){
        flipBit(g.individual, i)
      }
    }
  }

  def flipBit(g: Array[Char], idx: Int){
    if( g(idx) == '1' ) g(idx) = '0' else g(idx) = '1'
  }

  def incMutProb(m: Double){
    mProb += m
  }

}
