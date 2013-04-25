import scala.util.Random

trait PopulationGenerator {
  def generate[T](size: Int): Array[Genotype[T]]
}

class SnakePopulationGenerator(length: Int, dimension: Int) 
  extends PopulationGenerator
  {
  override def generate[T](size: Int): Array[Genotype[T]] = {
    var p = Array.ofDim[Genotype[T]](size)
    val rand = new Random

    for (i <- 0 until size) {
      var genes = Array.ofDim[Int](length)
      genes(0) = rand.nextInt(math.pow(2, dimension).toInt)
      // flip a bit at random from p(j-1)
      for (j <- 1 until length){
        genes(j) = genes(j-1)^math.pow(2, rand.nextInt(dimension)).toInt
      }
      p(i) = Genotype[T](genes.asInstanceOf[Array[T]])
    }
    p
  }
}

class SnakeNonPathPopulationGenerator(length:Int, dimension: Int)
  extends PopulationGenerator {
  override def generate[T](size: Int): Array[Genotype[T]] = {
    var p = Array.ofDim[Genotype[T]](size)
    val rand = new Random

    for (i <- 0 until size) {
      var genes = Array.ofDim[Int](length).map( 
        x => rand.nextInt(math.pow(2, dimension).toInt)
      )
      p(i) = Genotype[T](genes.asInstanceOf[Array[T]])
    }
   p 
  }
}

class SnakePermutationPopulationGenerator(length:Int, dimension: Int)
  extends PopulationGenerator {
  override def generate[T](size: Int): Array[Genotype[T]] = {
    var p = Array.ofDim[Genotype[T]](size)
    val rand = new Random
    for (i <- 0 until size) {
      var notChosen = Set[Int]()
      var genes = Array.ofDim[Int](length)
      (0 until math.pow(2,dimension).toInt).map (x => notChosen += x) 
      for (j <- 0 until length) {
        genes(j) = notChosen.toArray.apply(rand.nextInt(notChosen.size))
        notChosen = notChosen - genes(j)
      }
      p(i) = Genotype[T](genes.asInstanceOf[Array[T]])
    }
   p 
  }
}
