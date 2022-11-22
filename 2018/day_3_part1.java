import java.io.*;
import java.util.*;

public class AdventOfCode2018_day3_1 {
	
    public static void main(String[] args)throws IOException {
        
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        
        int[][] locations = new int[1000][1000];		//fabric
        for(int i = 0; i < 1000; i++) {
            Arrays.fill(locations[i], 0);		//everywhere starts with 0 claims
        }
        
        for(int i = 0; i < 1373; i++) {
            String s = br.readLine();
            String[] parts = s.split(" ");
            
            parts[2] = parts[2].substring(0, parts[2].length() - 1); //remove the colon
            String[] coords = parts[2].split(",");
            int x = Integer.parseInt(coords[0]);
            int y = Integer.parseInt(coords[1]);
            
            String[] dimensions = parts[3].split("x");
            int dx = Integer.parseInt(dimensions[0]);
            int dy = Integer.parseInt(dimensions[1]);
            
            for(int j = x; j < x + dx; j++) {
                for(int k = y; k < y + dy; k++) {
                    locations[j][k]++;		//increment number of claims over entire area
                }
            }
        }
        
        int total = 0;
        
		//find squares with more than 1 claim
        for(int i = 0; i < 1000; i++) {
            for(int j = 0; j < 1000; j++) {
                if(locations[i][j] > 1) {
                    total++;
                }
            }
        }
        System.out.println(total);
    }
}