import scala.util.Random

trait Operator {
  def operate(p: Array[_ <: Genotype])
}

//p is the crossover probability
case class TwoPointCrossover(p: Double) extends Operator {
  def operate (pop: Array[_ <: Genotype]) {
    val r = new Random()
    if (r.nextDouble() < p) {
      var i = 0
      while( i < pop.length - 1) {
        var g1 = pop(i)
        var g2 = pop(i+2)
        var cPoint1 = r.nextInt(g1.genes.length)
        var cPoint2 = r.nextInt(g2.genes.length)
        
        if (cPoint2 < cPoint1) {
          val temp = cPoint1
          cPoint1 = cPoint2
          cPoint2 = temp  
        }
        
        for (c <- cPoint1 to cPoint2) {
          val temp = g1.genes(c)
          g1.genes(c) = g2.genes(c)
          g2.genes(c) = temp 
        }
        i += 2
      }
    }
  }
}

object CrossoverTest extends App {
  var p = Array.ofDim[ForestSchedule](2)
  var g1 = ForestSchedule(Array(1, 2, 3, 4, 5))
  var g2 = ForestSchedule(Array(6, 7, 8, 9, 10))
  
  TwoPointCrossover(1) operate p
  p.foreach{ x => println(x.genes) }
}
