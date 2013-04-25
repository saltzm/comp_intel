import scala.util.Random

trait PopulationGenerator {
  def generate[T](size: Int): Array[Genotype[T]]
}

class ForestPlanningPopulationGenerator(nTimePeriods: Int, nStands: Int) 
  extends PopulationGenerator
  {
  override def generate[T](size: Int): Array[Genotype[T]] = {
    var p = Array.ofDim[Genotype[T]](size)
    val rand = new Random
    for (i <- 0 until size) {
      var genes = Array.ofDim[Int](nStands)
      for (j <- 0 until nStands){
        genes(j) = rand.nextInt(nTimePeriods + 1)
      }
      p(i) = Genotype[T](genes.asInstanceOf[Array[T]])
    }
    p
  }
}

object PopGenTest extends App {
  var p = new ForestPlanningPopulationGenerator(3, 73).generate[Int](1)
  println(p(0).genes.deep.mkString(","))
}
