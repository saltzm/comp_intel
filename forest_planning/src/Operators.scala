import scala.util.Random

trait Operator { def operate[T](p: Array[Genotype[T]]) }

case class UniformCrossover(p: Double) extends Operator {
  def operate[T](pop: Array[Genotype[T]]){
    val r = new Random
    var i = 0
    while (i < pop.size - 1) {
      var g1 = pop(i)
      var g2 = pop(i + 1)
      for (gene <- 0 until g1.genes.length) {
        if (r.nextDouble < p) {
          val temp = g1.genes(gene)
          g1.genes(gene) = g2.genes(gene)
          g2.genes(gene) = temp
        }
      }
      i += 2
    }
  }
}

//p is the crossover probability
case class TwoPointCrossover(p: Double) extends Operator {
  def operate[T](pop: Array[Genotype[T]]) {
    val r = new Random()
    if (r.nextDouble() < p) {
      var i = 0
      while( i < pop.size - 1) {
        var g1 = pop(i)
        var g2 = pop(i+1)
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

case class SimpleMutationForestPlanning(p: Double) extends Operator {
  def operate[T](pop: Array[Genotype[T]]) {
    var r = new Random()
    for (g <- pop) { 
      for (i <- 0 until g.genes.length) {
        if (r.nextDouble < p) {
          g.genes(i) = r.nextInt(4).asInstanceOf[T]
        }
      }
    }
  }
}

object SimpleMutationForestPlanningTest extends App {
  var p = Array.ofDim[Genotype[Int]](1)
  var g1 = Genotype[Int](Array(5, 5, 5, 5, 5))
  p(0) = g1
  SimpleMutationForestPlanning(0.2) operate p

  println(g1.genes.deep.mkString(","))
}

object CrossoverTest extends App {
  var p = Array.ofDim[Genotype[Int]](2)
  var g1 = new Genotype[Int](Array(1, 2, 3, 4, 5))
  var g2 = new Genotype[Int](Array(6, 7, 8, 9, 10))
 
  p(0) = g1
  p(1) = g2
 
  TwoPointCrossover(1).operate(p)
  p.foreach{ x => println(x.genes.deep.mkString(",")) }
}
