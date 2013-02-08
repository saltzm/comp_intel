package selection

import population.BitBasedPopulation
import genotype.Genotype

/**
 * Created with IntelliJ IDEA.
 * User: saltzm
 * Date: 1/20/13
 * Time: 9:39 PM
 * To change this template use File | Settings | File Templates.
 */
trait SelectionEngine {
  def select(population : BitBasedPopulation)
  def getTopIndividual() : Genotype
}
