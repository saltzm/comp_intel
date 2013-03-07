import collection.SortedSet
import crossover.{OnePointCrossoverEngine, TwoPointCrossoverEngine}
import fitness.{MFDFitnessFunction, SineFunction}
import io.Source
import mutation.SimpleMutationEngine
import selection.selection.ExclusiveTournamentSelectionEngine
import selection.{TournamentSelectionEngine, RouletteWheelSelectionEngine}
import solver.GASolver


object Main extends App {
  var genotypeLength = 25
  var lines = Source.fromFile("ExhaustiveResults10x25.txt").getLines()
  var exResults = Array.ofDim[Double](1024, 3)
  for (i <- 1 until 1024) {
    var parts = lines.next().trim.split( """\s+""")
    var first = parts(1)
    var second = parts(2)
    var third = parts(3)
    exResults(i)(0) = first.toDouble
    exResults(i)(1) = second.toDouble
    exResults(i)(2) = third.toDouble
  }

  var fitnessFunc = new MFDFitnessFunction("TendencyMatrix10x25.txt", 10, 25)

  //Experiment parameters
  var popSizes = Set(100)//Set(80, 120, 160)
  var crossProbs = Set(0.8)// Set(0.4, 0.6, 0.8, 1)
  var mutProbs = Set(.08)//Set(0.0125, 0.0167, 0.025, 0.033, 0.05, 0.066, 0.08, 0.1, 0.133, 0.2) 
  var elitism = Set(true)//Set(true,false)
  var start = 1
  var finish = 1024
  var repetitions = 1
  var nGens = 50

    runExperiment1("resultsRoulette"+System.currentTimeMillis()/10000+".csv")
    runExperiment2("resultsTourney"+System.currentTimeMillis()/10000+".csv")


  def runExperiment1(filename: String) {
    time {
      val out = new java.io.FileWriter(filename, true)
      for (popSize <- popSizes) {
        for (crossProb <- crossProbs) {
          for (mutProb <- mutProbs) {
            for (elitism <- elitism) {
              var crossEngine = new TwoPointCrossoverEngine(crossProb)
              var mutationEngine = new SimpleMutationEngine(mutProb)
              var noFirst = 0.0
              var noSnd = 0.0
              var noThrd = 0.0
              for (i <- 0 until repetitions) {
                for (symptomSet <- start until finish) {
//                  println(symptomSet)
                  var selector = new RouletteWheelSelectionEngine(elitism)
                  fitnessFunc.setMPlus(symptomSet)
                  var solver = new GASolver(popSize, genotypeLength, fitnessFunc, selector,
                    crossEngine, mutationEngine)
                  solver.solve(nGens)
                  if (math.abs(solver.getSolutionFitness() - exResults(symptomSet)(0)) < 1.0e-7) {
                    noFirst += 1
                  } else if (math.abs(solver.getSolutionFitness() - exResults(symptomSet)(1)) < 1.0e-7) {
                    noSnd += 1
                  } else if (math.abs(solver.getSolutionFitness() - exResults(symptomSet)(2)) < 1.0e-7) {
                    noThrd += 1
                  }
                  println(symptomSet)
                  //                println("sol fit:\t"+exResults(symptomSet)(0))
                  //                println("try fit:\t"+solver.getSolutionFitness())
                }
                println("run"+ i)
              }
              val firstPerc = ((noFirst / ((finish - start) * repetitions)) * 100)
              val secondPerc = ((noSnd / ((finish - start) * repetitions)) * 100)
              val thirdPerc = ((noThrd / ((finish - start) * repetitions)) * 100)


              out.write(popSize + "," + crossProb + "," + mutProb + "," + elitism + "," + firstPerc + "," + secondPerc + "," + thirdPerc + "\n")
              println("R1: " + popSize + "," + crossProb + "," + mutProb + "," + elitism + "," + firstPerc + "," + secondPerc + "," + thirdPerc)
              //            println("Pop. size: "+popSize+"\tCrossover Probability: "+crossProb+"\tmutProb: "+mutProb+"\telitism: "+elitism)
              //            println("Percentage in first: " + firstPerc+" %")
              //            println("Percentage in second: " + secondPerc +" %")
              //            println("Percentage in third: " + thirdPerc +" %")
            }
          }
        }
      }
      out.close()
    }
  }

  def runExperiment2(filename: String) {
    val tourneySizes = Set(2, 4, 8, 16)
//    var inProgress = 0

    for (popSize <- popSizes) {
      for (crossProb <- crossProbs) {
        for (mutProb <- mutProbs) {
          for (elitism <- elitism) {
            for (tourneySize <- tourneySizes) {
//              println(inProgress)

//              while(inProgress >= 4){ Thread.sleep(20000) }
//              inProgress +=1
//              thread {
                val out = new java.io.FileWriter(filename, true)
                val mr = mutProb//tourneySize/popSize.toDouble
               // println("tourneySize:  "+tourneySize)
                //println("popSize: "+popSize)
                //println("mr: "+mr)
                var crossEngine = new TwoPointCrossoverEngine(crossProb)
                //TODO:  look here
                var mutationEngine = new SimpleMutationEngine(mr) //tourneySize/popSize.toDouble)
                var noFirst = 0.0
                var noSnd = 0.0
                var noThrd = 0.0
                for (i <- 0 until repetitions) {
                  for (symptomSet <- start until finish) {
                    var selector = new TournamentSelectionEngine(tourneySize, 1, elitism)
                    fitnessFunc.setMPlus(symptomSet)
                    var solver = new GASolver(popSize, genotypeLength, fitnessFunc, selector,
                      crossEngine, mutationEngine)
                    solver.solve(nGens)
                    if (math.abs(solver.getSolutionFitness() - exResults(symptomSet)(0)) < 1.0e-7) {
                      noFirst += 1
                    } else if (math.abs(solver.getSolutionFitness() - exResults(symptomSet)(1)) < 1.0e-7) {
                      noSnd += 1
                    } else if (math.abs(solver.getSolutionFitness() - exResults(symptomSet)(2)) < 1.0e-7) {
                      noThrd += 1
                    }
                    //                println("sol fit:\t"+exResults(symptomSet)(0))
                    //                println("try fit:\t"+solver.getSolutionFitness())
                  }
                  println("run "+i)
                }
                val firstPerc = ((noFirst / ((finish - start) * repetitions)) * 100)
                val secondPerc = ((noSnd / ((finish - start) * repetitions)) * 100)
                val thirdPerc = ((noThrd / ((finish - start) * repetitions)) * 100)


                out.write(popSize + "," + crossProb + "," +mr/* mutProb*/ + "," + elitism + "," + tourneySize + "," + firstPerc + "," + secondPerc + "," + thirdPerc + "\n")
                println("R2: " + popSize + "," + crossProb + "," + mr + "," + elitism + "," + tourneySize + "," + firstPerc + "," + secondPerc + "," + thirdPerc)
                //            println("Pop. size: "+popSize+"\tCrossover Probability: "+crossProb+"\tmutProb: "+mutProb+"\telitism: "+elitism)
                //            println("Percentage in first: " + firstPerc+" %")
                //            println("Percentage in second: " + secondPerc +" %")
                //            println("Percentage in third: " + thirdPerc +" %")
//                inProgress -= 1
              out.close()

              //              }
            }
          }
        }
      }

    }

  }

  def time[R](block: => R) = {
    val t0 = System.currentTimeMillis()
    block // call-by-name
    val t1 = System.currentTimeMillis()
    println("Execution time: " + (t1 - t0) + " ms")
  }

  def thread[R](block: => R) = {
    new Thread(new Runnable {
      def run() {
        block
      }
    }).start()
  }
}




