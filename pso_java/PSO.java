import java.lang.Math;
import java.nio.file.*;
import java.nio.charset.*;
import java.io.IOException;
import java.io.BufferedReader;

public class PSO {
    public static void main(String[] args) { 
        MFDFitnessFunction fitnessFunc = 
            new MFDFitnessFunction("TendencyMatrix10x25.txt", 10, 25);
        double[][] exResults = readExResults("ExhaustiveResults10x25.txt");
        int nDis = 25, nGens = 500, inReps = 10, noReps = 10, start = 1, finish = 1024;
        double threshold = 1e-7;


        int[] popSizes;         
        double[] c1s;           
        double[] c2s;           
        double[] vMins;         
        double[] vMaxs;         
        double[] ws;            
        double[] gensToConverge;


        if (args[0].equals("classic") ) {
            popSizes         = new int[]{120};//int[]{40, 60, 80};
            c1s              = new double[]{2};//{2, 1.75, 1.5, 1, 0.5, 0};
            c2s              = new double[]{2};//{2, 2.25, 2.5, 3, 3.5, 4};
            vMins            = new double[]{-4};//{-4, -6};
            vMaxs            = new double[]{4};//, 6};
            ws               = new double[]{1};//, 0.75, 0.5, 0.25, 0};
            gensToConverge   = new double[]{40};//, 5, 10};
        } else { 
            popSizes         = new int[]{20};//, 80, 1000};
            c1s              = new double[]{3};//{2, 1.75, 1.5, 1, 0.5, 0};
            c2s              = new double[]{2};//{2, 2.25, 2.5, 3, 3.5, 4};
            vMins            = new double[]{-3};//{-4, -6};
            vMaxs            = new double[]{3};//, 6};
            ws               = new double[]{0};//, 0.75, 0.5, 0.25, 0};
            gensToConverge   = new double[]{10};//, 10, 30, 50, 100};
        } 
        for (int gc = 0; gc < gensToConverge.length; gc++) {
        for (int wc = 0; wc < ws.length; wc++) {
        for (int vc = 0; vc < vMins.length; vc++) {
        for (int cc = 0; cc < c1s.length; cc++) {
        for (int pc = 0; pc < popSizes.length; pc++){
            long t0 = System.currentTimeMillis();
            int stopGen = 0, noFirst = 0, noSecond = 0, noThird = 0;
            //repeat noReps times
            for (int i = 0; i < noReps; i++) {
                //for each symptom set
                for(int sympSet = start; sympSet < finish; sympSet++) {
                    double maxGBest = 0;
                    for (int n = 0; n < inReps; n++) {
                        
                        //System.out.println(sympSet);
                        fitnessFunc.setMPlus(sympSet);
                        PartPop pop = new PartPop(popSizes[pc], nDis, c1s[cc], c2s[cc], 
                                                  vMins[vc], vMaxs[vc], ws[wc], fitnessFunc);
                        int genSame = 0;
                        //update entire population for nGens or until convergences
                        for (int gen = 0; gen < nGens; gen++) {
                            for( Particle part : pop.pop) {
                                if (args[0].equals("classic"))
                                    part.update(pop.gBest, pop.gBestFit, "classic");
                                else part.update(pop.gBest, pop.gBestFit, "new"); 
                                if (part.fitness > pop.gBestFit) {
                                    pop.gBest = part.x.clone();
                                    pop.gBestFit = part.fitness;
                                    genSame = 0;
                                }
                            }
                            genSame++;
                            if(genSame == gensToConverge[gc] || gen == nGens - 1) {stopGen += gen+1; break;}
                        }
                        if (pop.gBestFit > maxGBest) maxGBest = pop.gBestFit; 
                    }
                    //System.out.println(pop.gBestFit);
                    //System.out.println("gens:\t"+stopGen);
                    if (Math.abs(maxGBest - exResults[sympSet][0]) < threshold){ 
                        noFirst++;
                    }
                    //else if (Math.abs(pop.gBestFit - exResults[sympSet][1]) < threshold)
                        //noSecond++;
                    //else if (Math.abs(pop.gBestFit - exResults[sympSet][2]) < threshold)
                        //noThird++;
                }
            }
            long t1 = System.currentTimeMillis();
            double rel1 = ((double) noFirst)/(double) (noReps*(finish-start));
            double rel2 = ((double) noSecond)/(double) (noReps*(finish-start));
            double rel3 = ((double) noThird)/(double) (noReps*(finish-start));
            double avStopGen = ((double) stopGen)/(double) (noReps*(finish-start));
            System.out.println(gensToConverge[gc]+","+popSizes[pc] +"," + c1s[cc] + "," 
                    + c2s[cc] + "," 
                    + vMins[vc] +"," + vMaxs[vc] +"," + ws[wc] + "," + avStopGen +","
                    + rel1 + "," + rel2 + "," + rel3 + "," + (t1-t0));
        }
        }
        }
        }
        }
        }

    public static double[][] readExResults(String name) {
        double[][] exResults = new double[1024][3];
        Path p = FileSystems.getDefault().getPath("/Users", "saltzm", "Documents", "comp_intel", "comp_intel", "pso_java", name);
        Charset charset = Charset.forName("US-ASCII");
        try (BufferedReader in = Files.newBufferedReader(p, charset)) {
            for (int i = 1; i < 1024; i++) {
                String[] parts = in.readLine().trim().split("[ ]+");
                exResults[i][0] = Double.parseDouble(parts[1]);
                exResults[i][1] = Double.parseDouble(parts[2]);
                exResults[i][2] = Double.parseDouble(parts[3]);
            }
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(-1);
        }
       return exResults; 
    }
}
