import java.util.*;
import java.io.*;

public class AdventOfCode2018_6_1 {
    
    public static void main(String[] args)throws IOException {       
        
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        int[] scores = new int[50];
        ArrayList<Pair> pairs = new ArrayList<>();
        
        for(int i = 0; i < 50; i++) {
            String[] parts = br.readLine().split(", ");
            int a = Integer.parseInt(parts[0]);
            int b = Integer.parseInt(parts[1]);
            
            pairs.add(i, new Pair(a + 1500, b + 1500));     //shift by 250
        }
        Arrays.fill(scores, 1);
        
        for(int i = 0; i < 3000; i++) { 
            for(int j = 0; j < 3000; j++) {
                int min = 20000, save = -1;
                boolean hold = false;
                for(int k = 0; k < pairs.size(); k++) {
                    Pair p = pairs.get(k);
                    int distance = Math.abs(p.a - i) + Math.abs(p.b - j);
                    if (distance == min) {
                        hold = true;
                    }
                    if (distance < min) {
                        hold = false;
                        min = distance;
                        save = k;
                    }
                }
                if(!hold) {
                    scores[save]++;
                }
            }
        }
        
        int min = 1000000;
        
        for(int i: scores) {
            System.out.print(i + " ");
            if(i < min) {
                min = i;
            }
        }
        System.out.println("\n" + min);
    }
}

class Pair {
    int a, b;
    
    public Pair(int a, int b) {
        this.a = a;
        this.b = b;
    }
}