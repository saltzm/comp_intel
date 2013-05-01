import scala.util.Random

trait PopulationGenerator {
  def generate[T](size: Int): Array[Genotype[T]]
}

class TransitionPopulationGenerator(length:Int, dimension: Int)
  extends PopulationGenerator {
  override def generate[T](size: Int): Array[Genotype[T]] = {
    // population with 'size' genotypes
    var p = Array.ofDim[Genotype[T]](size)
    // random numb generator
    val rand = new Random
    
    // for each genotype
    for (i <- 0 until size) {
      // create an array of genes which consists of transition
      // steps numbered from 0 to dimension - 1
      var genes = Array.ofDim[Int](length).map( 
        x => rand.nextInt(dimension)
      )
      // set the current member of this population to be a new
      // Genotype containing these genes
      p(i) = Genotype[T](genes.asInstanceOf[Array[T]])
    }
   p 
  }
}
