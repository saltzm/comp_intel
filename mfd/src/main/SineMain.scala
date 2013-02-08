package main

import crossover.OnePointCrossoverEngine
import mutation.SimpleMutationEngine
import selection.{RouletteWheelSelectionEngine, TournamentSelectionEngine}
import fitness.SineFunction
import solver.GASolver
import selection.selection.ExclusiveTournamentSelectionEngine


object SineMain extends App{
  var genotypeLength = 8
  var crossEngine = new OnePointCrossoverEngine(0.3)
  var mutationEngine = new SimpleMutationEngine(0.01)
  var popSize = 10
  var selector = new TournamentSelectionEngine(4, 1, true) //new RouletteWheelSelectionEngine(false)
  var fitnessFunc = new SineFunction()


  var solver = new GASolver(popSize, genotypeLength, fitnessFunc, selector,
    crossEngine, mutationEngine, true)
  solver.solve(60)

}
