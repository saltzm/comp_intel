
import java.util.Random;
import java.lang.Math;

public class Particle {
   
    int dims;
    double c1, c2, vMin, vMax, w;
    double fitness, pBestFit;
    byte[] x, pBest;
    double[] v;
    MFDFitnessFunction fitFunc;
    Random rand; 

    public Particle (int dims, double c1, double c2, double vMin, double vMax, 
                     double w, MFDFitnessFunction fitFunc) {
        this.dims = dims; 
        this.c1 = c1; this.c2 = c2; this.vMin = vMin; this.vMax = vMax; this.w = w;
        this.fitFunc = fitFunc;
        this.rand = new Random();
        x = new byte[dims];
        v = new double[dims];
        initializeX();
        initializeV();
        this.fitness = calculateFitness();
        this.pBest = x.clone();
        this.pBestFit = fitness;
    }
   
    private void initializeX() {
        for (int i = 0; i < x.length; i++) { 
            x[i] = (byte) rand.nextInt(2); 
        }
    }

    private void initializeV() {
        for (int i = 0; i < v.length; i++)
            v[i] = rand.nextDouble() * vMax * Math.pow((-1), rand.nextInt(2));
    }

    public double calculateFitness() {
        return fitFunc.calculateFitness(x);
    }

    public void update(byte[] gBest, double gBestFit, String type) {
        for (int d = 0; d < dims; d++) {
            v[d] = Math.min(w * v[d]
                 + c1 * rand.nextDouble() * (pBest[d] - x[d])
                 + c2 * rand.nextDouble() * (gBest[d] - x[d]), vMax);
            v[d] = Math.max(v[d], vMin);
            if (type.equals("classic"))
                x[d] = classicUpdate(v[d]);
            else x[d] = newUpdate(v[d]); 
        }
        this.fitness = calculateFitness();
        if (fitness > pBestFit) {
            pBest = x.clone();
            pBestFit = fitness;
       }
    }

    public byte newUpdate(double v) {
        return (byte) (((int) v) % 2);
    }

    public byte classicUpdate(double v){
        double vMod = 1.0 / (1 + Math.pow(Math.E, (-1) * v));
        if (rand.nextDouble() < vMod) return 1;
        else return 0;
    }

}
