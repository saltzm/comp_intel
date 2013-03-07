

public class PartPop {

    byte[] gBest; 
    double gBestFit; 
    Particle[] pop;

    public PartPop (int popSize, int dims, double c1, double c2,
                    double vMin, double vMax, double w, 
                    MFDFitnessFunction fitFunc) {
        pop = new Particle[popSize];
        pop[0] = new Particle (dims, c1, c2, vMin, vMax, w, fitFunc);
        gBest = pop[0].x;
        gBestFit = pop[0].fitness;

        for (int i = 0; i < pop.length; i++) {
            pop[i] = new Particle (dims, c1, c2, vMin, vMax, w, fitFunc); 
            if (pop[i].fitness > gBestFit) {
                gBest = pop[i].x;
                gBestFit = pop[i].fitness;
            }
        }
    }
}
