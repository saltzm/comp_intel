import scala.io.Source
import scala.util.Random
import scala.collection.mutable.Map


class HybridFitnessFunction (dimension: Int) extends FitnessFunction { 
  //adjacency lists
  val adjacencies = calculateAdjacencies(dimension)

  //stand volumes per time period
  override def getFitness[T](g: Genotype[T]): Double = {
    val genes = g.genes.map( x => x.asInstanceOf[Int] )
    var (snake, length) = buildSnake(genes)
for (i <- 0 until length + 1) {
      genes(i) = snake(i) 
    }
    length //- math.log(countViolations(genes))
    //length/math.log(countViolations(genes))
    //length/countViolations(genes).toDouble
    //buildSnake(genes)//countViolations(genes) * -1
  }

  def calculateAdjacencies(dimension: Int): Array[Set[Int]] = {
    var adj = Array.ofDim[Set[Int]](math.pow(2, dimension).toInt).map(
      x => Set[Int]()
    )
    for(i <- 0 until adj.length) {
      for(dim <- 0 until dimension) {
        adj(i) = adj(i) + (i^math.pow(2, dim).toInt)
      }
    }
    adj
  }

  def buildSnake(genes: Array[Int]): (Array[Int], Int) = {
    val snake = Array.ofDim[Int](genes.length)
    val rand = new Random
    var geneSet = genes.toSet
    var skins = Set[Int]()
    var done = false
    var grown = false
    var length = 0
    snake(0) = genes(0)
    geneSet -= genes(0)
    //val first = rand.nextInt(math.pow(2, dimension).toInt)
    //snake(0) = genes(first)
    //geneSet -= genes(first)
    for(i <- 1 until snake.length if !done) {
     /* println("geneSet: " + geneSet)*/
      //println("skins: " + skins)
      /*println(snake.deep.mkString(","))*/
      //TODO make sure it's only one value
      //println(i + ": "+adjacencies(snake(i-1)))
      val nextSet = (adjacencies(snake(i-1)) & geneSet) &~ skins
      if (nextSet.size != 0) {
        //println("n "+
        //val nextVal = nextSet.toArray.apply(rand.nextInt(nextSet.size))
        val nextVal = nextSet.iterator.next   
        geneSet = geneSet - nextVal
        skins |= adjacencies(snake(i-1))
        snake(i) = nextVal 
        length += 1
      } else if (!grown){
        Array.range(0, 255).map( x => geneSet = geneSet + x)
        grown = true
      } else {
        done = true
      }
    }
    (snake, length)//snake
  }

  def countViolations(genes: Array[Int]): Int = {
    var count = 0
    var skins = Set[Int]()
    var visited = Set[Int]()
    for (i <- 0 until genes.length) {
      if (skins contains genes(i)) { 
        count += 1
      } 
      if (visited contains genes(i)) {
        count += 2
      }
      visited = visited + genes(i)
      skins = (skins union adjacencies(genes(i)))
    }
    return count// - (genes.length - 1) 
  }
}

