#!/bin/bash

(time scala -cp out MSESolver 1000 10 672 1495) > ga_results_672_1495 2>&1
(time scala -cp out MSESolver 1000 10 200 1000) > ga_results_200_1000 2>&1
(time scala -cp out MSESolver 1000 10 700 50) > ga_results_700_50 2>&1

(time scala -cp out PSO_Solver 1000 10 672 1495) > results/pso_results/pso_results_672_1495 2>&1
(time scala -cp out PSO_Solver 1000 10 200 1000) > results/pso_results/pso_results_200_1000 2>&1
(time scala -cp out PSO_Solver 1000 10 700 50) > results/pso_results/pso_results_700_50 2>&1
