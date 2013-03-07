
case class MSEGenotype(genes: Array[Int], fitFunc: MSEFitnessFunction){ 
  val fitness = fitFunc.calculateFitness (this)
  def nc = genes(0)
  def len = genes(1)
  def s1 = genes(2)
  def s2 = genes(3)
  def scc = genes(4)
  def rau = genes(5)
  def nai = genes(6)
}
