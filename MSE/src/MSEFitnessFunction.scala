
class MSEFitnessFunction(MSRT_needed: Int, DNVT_needed: Int) {

  val corps = Array[Int] (42, 9, 168, 56, 7, 92, 4)
  val percent = 0.142857 //each component represents 1/7
  val divs = Array[Int] (4, 1, 12, 4, 1, 9, 1)
  val nc_wire = 24
  val len_wire = 176
  val s1_wire = 26
  val s2_wire = 41
  val rau_radio = 25

  def calculateFitness (g: MSEGenotype): Double = {
    // multiply together seven factors  
    val a = cardinality(g) 
    val b = sen1ToSen2(g) 
    val c = uhfConnectivity(g)  
    //val d = minConstraintViolations(g) 
    //val e = maxConstraintViolations(g) 
    val f = msrtSupport(g) 
    val q = dnvtSupport(g)
   /* println("a: " + a)*/
    //println("b: " + b)
    //println("c: " + c)
    //println("d: " + d)
    //println("e: " + e)
    //println("f: " + f)
    /*println("q: " + q)*/
    a * b * c * /*d * e* */ f * q
  }

  def cardinality (g: MSEGenotype): Double = {
    var sum = 0.0
    for (i <- 0 until g.genes.length) { 
      sum += g.genes(i).toDouble/corps(i)
    }
    50 / (sum * percent) 
  }

  def sen1ToSen2 (g: MSEGenotype): Double = {
    if (g.s1 > 3 * g.s2) 3.0 * g.s2 / g.s1
    else g.s1 / (3.0 * g.s2)
  }

  def uhfConnectivity (g: MSEGenotype): Double = {
    val x = ((g.nc - 1) % 4) + 1
    val y = (g.nc - 1) / 4              //TODO: change to double division?
    val z = y + 1
    val total_available = g.nc * 12
    val available_from_backbone = (32 * y) - (x * x) + (13 * x)
    val others = g.len * 2 + g.genes.slice (2, g.genes.length).sum 
    val needed = total_available - available_from_backbone + others 
    
    if (total_available - needed > 12) needed.toDouble / total_available
    else if (total_available < needed) 0
    else 1
  }

  def minConstraintViolations (g: MSEGenotype): Double = {
    if (g.nc < 1 || g.scc < 1) 0
    else 1
  }

  def maxConstraintViolations (g: MSEGenotype): Double = {
    val z = (g.nc - 1) / 4 + 1             //TODO: change to double division?
    for (i <- 0 until g.genes.length) //TODO: Antennas?
      if (g.genes(i) > divs(i) * z) 
        return 0
    1
  }

  def dnvtSupport (g: MSEGenotype): Double = {
    val supported = g.nc * nc_wire +
                    g.len * len_wire + 
                    g.s1 * s1_wire +
                    g.s2 * s2_wire
    if (supported < DNVT_needed) 0
    else DNVT_needed.toDouble / supported
  }

  def msrtSupport (g: MSEGenotype): Double = {
    val supported = g.rau * rau_radio 
    if (supported < MSRT_needed) 0
    else MSRT_needed.toDouble / supported
  }
}

object FitnessTest extends App {
  val fitFunc = new MSEFitnessFunction(672, 1495)
  var g = new MSEGenotype(Array[Int](9, 1, 30, 9, 1, 27, 1), fitFunc)
  println(g.fitness)
  g = new MSEGenotype(Array[Int](9, 2, 25, 8, 1, 27, 1), fitFunc)
  println(g.fitness)
  g = new MSEGenotype(Array[Int](9, 1, 27, 10, 1, 27, 1), fitFunc)
  println(g.fitness)
  g = new MSEGenotype(Array[Int](9, 1, 28, 10, 1, 27, 1), fitFunc)
  println(g.fitness)
  g = new MSEGenotype(Array[Int](9, 1, 29, 10, 1, 27, 1), fitFunc)
  println(g.fitness)
  g = new MSEGenotype(Array[Int](9, 1, 30, 10, 1, 27, 1), fitFunc)
  println(g.fitness)
  g = new MSEGenotype(Array[Int](14, 0, 31, 11, 0, 28, 0), fitFunc)
  println(g.fitness)

}
