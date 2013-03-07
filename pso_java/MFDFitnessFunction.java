import java.nio.file.*;
import java.nio.charset.Charset;
import java.io.BufferedReader;
import java.io.IOException;


public class MFDFitnessFunction implements FitnessFunction {
 
    byte[] mPlus;
    double[] priorProb;
    double[][] tendencyMatrix;
    final double threshold = 1.0e-7;
    int nSympt;

    public MFDFitnessFunction (String matFileName, int nSympt, int nDis) {
        this.nSympt = nSympt;
        this.priorProb = new double[nDis];
        this.tendencyMatrix = readTendencyMatrix(matFileName, nDis, nSympt);
    }

    private double[][] readTendencyMatrix(String name, int nDis, int nSympt) {
        Path p = FileSystems.getDefault().getPath(name);
        double[][] tendencyMatrix = new double[nSympt][nDis];
        Charset charset = Charset.forName("US-ASCII");
        try (BufferedReader in = Files.newBufferedReader(p, charset)) {
            for (int i = 0; i < nDis; i++) 
                this.priorProb[i] = Double.parseDouble(in.readLine());
            for (int i = 0; i < nSympt; i++) { 
                in.readLine(); //consume empty line
                for (int j = 0; j < nDis; j++) {
                    tendencyMatrix[i][j] = Double.parseDouble(in.readLine());
                }
            }
        } catch(IOException e) {
            e.printStackTrace();
            System.exit(-1);
       }
        return tendencyMatrix;
    }

    private byte[] intToByteArray(int n, int nSympt) {
        String s = Integer.toBinaryString(n);
       
        while(s.length() < nSympt) s = "0" + s;
        byte[] res = new byte[nSympt];
        for (int i = 0; i < nSympt; i++) {
            if (s.charAt(i) == '1') res[i] = 1;
            else res[i] = 0;
        }
        return res;
    }

    public void setMPlus(int mPlus) { this.mPlus = intToByteArray(mPlus, nSympt); }

    public double calculateFitness (byte[] g) {
        return l1(g) * l2(g) * l3(g);
    }

    public double l1 (byte[] g) {
        double l1 = 1.0;
        for (int i = 0; i < mPlus.length; i++) {
            double dProd = 1.0;
            if (mPlus[i] == 1) {
                for (int j = 0; j < g.length; j++){ 
                    if (g[j] == 1) {
                        dProd *= (1.0 - tendencyMatrix[i][j]);
                    }
                }
                l1 *= (1 - dProd);
            }
        }
        return l1;
    }

    public double l2 (byte[] g) {
        double l2 = 1.0;
        for (int j = 0; j < g.length; j++) {
            double mProd = 1.0;
            if (g[j] == 1) { 
                for (int l = 0; l < mPlus.length; l++) {  
                    if ((mPlus[l] == 0) && (tendencyMatrix[l][j] > threshold)) {
                        mProd *= (1.0 - tendencyMatrix[l][j]);
                    }
                }
                l2 *= mProd;
            }            
        }
        return l2;
    }
    
    public double l3 (byte[] g){
        double l3 = 1.0;
        for (int j = 0; j < g.length; j++) {
            if (g[j] == 1){
                l3 *= (priorProb[j] / (1.0 - priorProb[j]));
            }
        }
        return l3;
    }
}
