
class Genotype(_genes: Array[_ <: Any]) {
  var genes = _genes
}

case class ForestSchedule(gs: Array[Int]) extends Genotype(gs)
