
case class MidtermGenotype(genes: Array[Int], fitFunc: MidtermFitnessFunction){ 
  val fitness = fitFunc.calculateFitness (this)
}
