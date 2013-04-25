import scala.io.Source
import scala.collection.mutable.Map

class PSOForestPlanningFitnessFunction (adjacencyFile: String, 
                                     volumeFile: String,
                                     numberOfStands: Int,
                                     numberOfTimePeriods: Int,
                                     target: Int) { 
  //adjacency lists
  val adjacencies = readAdjacencies(adjacencyFile, numberOfStands)
  val (acres, volumes) = readVolumeFile(volumeFile, numberOfStands, 
                                        numberOfTimePeriods)

  //stand volumes per time period
  def calculateFitness(p: Particle): Double = {
    calculateAndSaveFitness(p)
  }

  def calculateAndSaveFitness(p: Particle): Double = {
    val genes = p.x.map( x => x.asInstanceOf[Int] )
    val adjViolations = countViolations(genes)
    var hArray = Array.ofDim[Double](numberOfTimePeriods)

    for (stand <- 0 until genes.length) {
      // genes are numbered with 0 being no cut whereas these are numbered 
      // with 0 as first period
      val t = genes(stand) - 1    
      if (t >= 0) {
        hArray(t) += acres(stand) * volumes(stand)(t)
      }
    }
    var fitness = 0.0 
    for (i <- 0 until numberOfTimePeriods) {
      //println("Period "+i+":\t"+ hArray(i))
      fitness += math.pow(hArray(i) - target, 2)
    }
    //TODO: improve adjacency penalty
    fitness += math.log(adjViolations + 1) * numberOfTimePeriods * target*target
    //if(adjViolations > 0) fitness +=  numberOfTimePeriods * target*target
    fitness *= -1
    //fitnesses += (genes.deep -> fitness)
    fitness
  }

  def getPeriodHarvests(p: Particle): Array[Double] = {
    val genes = p.x.map( x => x.asInstanceOf[Int] )
    var hArray = Array.ofDim[Double](numberOfTimePeriods)

    for (stand <- 0 until genes.length) {
      val t = genes(stand) - 1 
      if (t >= 0) {
        hArray(t) += acres(stand) * volumes(stand)(t)
      }
    }
    hArray
  }

  def countViolations(genes: Array[Int]): Int = {
    var count = 0
    for (i <- 0 until genes.length) {
      val t = genes(i)
      for (neighbor <- adjacencies(i)) {
        if (genes(neighbor) == t) {
          count += 1
        }
      }
    }
    return count/2 
  }

  def readAdjacencies(filename: String, nStands: Int): Array[Set[Int]] = {
    val lines = Source.fromFile(filename).getLines
    val adj = Array.ofDim[Set[Int]](nStands).map(x => Set[Int]())
    for (line <- lines) {
      val lArr = line.split(',').map(_.toInt - 1)
      adj(lArr(0)) += lArr(1) 
    }
    adj
  }

  def readVolumeFile(filename: String, nStands: Int, nPeriods: Int): 
            (Array[Double], Array[Array[Double]]) = {
    val lines = Source.fromFile(filename).getLines
    val acres = Array.ofDim[Double](nStands)
    val volumes = Array.ofDim[Array[Double]](nStands).map( 
                    x => Array.ofDim[Double](nPeriods)
                  )
    for (line <- lines) {
      val lArr = line.split(',')
      val stand = lArr(0).toInt - 1
      acres(stand) = lArr(1).toDouble
      for (i <- 0 until nPeriods) {
        volumes(stand)(i) = lArr(2 + i).toDouble
      }
    }
    (acres, volumes)
  }
}
