import scala.io.Source
import scala.util.Random
import scala.collection.mutable.Map

trait FitnessFunction { def getFitness[T](g: Genotype[T]): Double }

class LengthFitnessFunction (dimension: Int) extends FitnessFunction { 
  //adjacency lists
  val adjacencies = Fitness.calculateAdjacencies(dimension)

  def buildSnake[T](g: Genotype[T]): (Double, Int) = { 
    var genes = g.genes.asInstanceOf[Array[Int]]
    // starting node is 0
    var curNode = 0
    var length = -1 
    // for each node
    //    -add neighbor by flipping bit corresponding to the
    //    transition number
    //    -add neighbors of previous node to skins
    var skins = Set[Int]()
    var prevNode = -1 
    for (gene <- genes if (!skins.contains(curNode))) {
      skins = skins + curNode
      length += 1
      if (prevNode != -1) {
        adjacencies(prevNode).map( x => (skins = skins + x))
      }
      prevNode = curNode
      curNode = prevNode^math.pow(2, gene).toInt 
    }
    (length, length)
  }

  override def getFitness[T](g: Genotype[T]): Double = {
    buildSnake(g)._1
  }
}

class SkinsFitnessFunction (dimension: Int) extends FitnessFunction { 
  //adjacency lists
  val adjacencies = Fitness.calculateAdjacencies(dimension)

  override def getFitness[T](g: Genotype[T]): Double = {
    buildSnake(g)._1
  }

  def buildSnake[T](g: Genotype[T]): (Double, Int) = {
    var genes = g.genes.asInstanceOf[Array[Int]]
    // starting node is 0
    var curNode = 0
    var length = -1 
    // for each node
    //    -add neighbor by flipping bit corresponding to the
    //    transition number
    //    -add neighbors of previous node to skins
    var skins = Set[Int]()
    var prevNode = -1 
    for (gene <- genes if (!skins.contains(curNode))) {
      skins = skins + curNode
      length += 1
      if (prevNode != -1) {
        adjacencies(prevNode).map( x => (skins = skins + x))
      }
      prevNode = curNode
      curNode = prevNode^math.pow(2, gene).toInt 
    }
    (length - math.log(skins.size+1), length)
  }
}

class ViolationFitnessFunction (dimension: Int) extends FitnessFunction { 
  //adjacency lists
  val adjacencies = Fitness.calculateAdjacencies(dimension)

  override def getFitness[T](g: Genotype[T]): Double = {
    buildSnake(g)._1
  }

  def buildSnake[T](g: Genotype[T]): (Double, Int) = {
    var genes = g.genes.asInstanceOf[Array[Int]]
    // starting node is 0
    var curNode = 0
    var length = -1 
    var violations = 0
    // for each node
    //    -add neighbor by flipping bit corresponding to the
    //    transition number
    //    -add neighbors of previous node to skins
    var skins = Set[Int]()
    var prevNode = -1 
    var snakeDone = false
    for (gene <- genes ) {
      if (!skins.contains(curNode) && !snakeDone) {
        skins = skins + curNode
        length += 1
        if (prevNode != -1) {
          adjacencies(prevNode).map( x => (skins = skins + x))
        }
      } else if (snakeDone) {
        if (skins.contains(curNode)) {
          violations += 1
        }
      } else {
        snakeDone = true
      }
      prevNode = curNode
      curNode = prevNode^math.pow(2, gene).toInt 
    }
    (length - math.log(violations + 1), length)
  }
}
object Fitness {
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
}

/*object FitnessTest extends Application {*/
  //var g = Genotype[Int](Array[Int](4, 3, 1, 2))
  //val fitFunc = new Tr
//}

