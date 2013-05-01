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
case class RandomCrossover(p: Double) extends Operator {
  def operate[T](pop: Array[Genotype[T]]){
    val r = new Random
    var i = 0
    while (i < pop.size - 1) {
      var g1 = pop(i)
      var g2 = pop(i + 1)
      for (gene <- 0 until g1.genes.length) {
        if (r.nextDouble < p) {
          g1.genes(gene) = g2.genes(r.nextInt(g1.genes.length))
          g2.genes(gene) = g1.genes(r.nextInt(g1.genes.length))
        }
      }
      i += 2
    }
  }
}
case class AdaptiveCrossover(p: Double) extends Operator {
  def operate[T](pop: Array[Genotype[T]]){
    val r = new Random
    var i = 0
    while (i < pop.size - 1) {
      var g1 = pop(i)
      var g2 = pop(i + 1)
      for (gene <- 0 until g1.genes.length) {
        if (r.nextDouble < 1.0/math.pow(2, g1.genes.length - gene)) {
          val temp = g1.genes(gene)
          g1.genes(gene) = g2.genes(gene)
          g2.genes(gene) = temp
        }
      }
      i += 2
    }
  }
}
case class OnePointCrossover(p: Double) extends Operator {
  def operate[T](pop: Array[Genotype[T]]) {
    val r = new Random()
    if (r.nextDouble() < p) {
      var i = 0
      while( i < pop.size - 1) {
        var g1 = pop(i)
        var g2 = pop(i+1)
        var cPoint1 = r.nextInt(g1.genes.length)
        
        for (c <- cPoint1 until g2.genes.length) {
          val temp = g1.genes(c)
          g1.genes(c) = g2.genes(c)
          g2.genes(c) = temp 
        }
        i += 2
      }
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
case class AdaptiveMutationSnake(p: Double, dimension: Int) extends Operator {
  def operate[T](pop: Array[Genotype[T]]) {
    var r = new Random()
    for (g <- pop) { 
      //want the first node to always be 0
      for (i <- 1 until g.genes.length) {
        if (r.nextDouble < 1.0/math.pow(2, g.genes.length - i)) {
          g.genes(i) = r.nextInt(math.pow(2, dimension).toInt).asInstanceOf[T]
        }
      }
    }
  }
}

case class TransitionMutationSnake(p: Double, dimension: Int) extends Operator {
  def operate[T](pop: Array[Genotype[T]]) {
    var r = new Random()
    for (g <- pop) { 
      //want the first node to always be 0
      for (i <- 0 until g.genes.length) {
        if (r.nextDouble < p) {
          g.genes(i) = r.nextInt(dimension).asInstanceOf[T]
        }
      }
    }
  }
}

