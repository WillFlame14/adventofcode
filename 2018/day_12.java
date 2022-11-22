import java.util.*;
import java.io.*;

public class AdventOfCode2018_12 {
    
    public static void main(String[] args)throws IOException {       
        
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        
        String initial = br.readLine().substring(15);
        br.readLine();
        HashMap<String, String> hs = new HashMap<>(32);
        
        for(int i = 0; i < 32; i++) {
            String[] parts = br.readLine().split(" ");
            
            hs.put(parts[0], parts[2]);
        }
        
        String next = "...." + initial + "....";
        String last = next;
        long subtractions = 0;
        String save = next;
        long lastn = 0;
        
        System.out.println(31113 + (75*(50E9 - 400)));
        
        for(int generations = 1; generations <= 200; generations++) {
            next = save;
            last = save;
            subtractions = 0;
            for(int i = 0; i < generations; i++) {
                for(int j = 0; j < last.length() - 4; j++) {
                    String temp = last.substring(j, j + 5);
    //                System.out.println("looking at " + temp);

                    if(hs.containsKey(temp)) {
                        next = next.substring(0, j + 2) + hs.get(temp) + next.substring(j+3);
    //                    System.out.println("triggered index " + j);
                    }
                    else {
                        if(last.charAt(j + 2) == '#') {     //plant dies
                            next = next.substring(0, j + 2) + "." + next.substring(j+3);
                        }
                    }
                }
//                System.out.println(next);
                next = "...." + next + "....";
                while(next.startsWith(".....")) {
                    next = next.substring(1);
                    subtractions++;
                }
                while(next.endsWith(".....")) {
                    next = next.substring(0, next.length() - 1);
                }

                last = next;
            }

            long total = 0;

            for(int i = 0; i < last.length(); i++) {
                if(last.charAt(i) == '#') {
                    total += (i - 4*(generations + 1)) + subtractions;
                }
            }
            System.out.println(generations + " " + total + " " + (total - lastn));
            lastn = total;
        }
    }
}