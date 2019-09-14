import java.io.*;
import java.util.*;

public class AdventOfCode2018_day3_2 {
	
    public static void main(String[] args)throws IOException {
        
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        
        int[][] locations = new int[1000][1000];		//fabric
        for(int i = 0; i < 1000; i++) {
            Arrays.fill(locations[i], 0);		//everywhere starts with 0 claims
        }
        
        Pair[] coordss = new Pair[1373];		//arrays with all coordinates and dimensions
        Pair[] dimensionss = new Pair[1373];
        
        int save = -1;
        
        for(int i = 0; i < 1373; i++) {
            String s = br.readLine();
            String[] parts = s.split(" ");
            
            parts[2] = parts[2].substring(0, parts[2].length() - 1); //remove the colon
            String[] coords = parts[2].split(",");
            int x = Integer.parseInt(coords[0]);
            int y = Integer.parseInt(coords[1]);
            coordss[i] = new Pair(x, y);		//add to list of all coords
            
            String[] dimensions = parts[3].split("x");
            int dx = Integer.parseInt(dimensions[0]);
            int dy = Integer.parseInt(dimensions[1]);
            dimensionss[i] = new Pair(dx, dy);		//add to list of all dimensions
            
            for(int j = x; j < x + dx; j++) {
                for(int k = y; k < y + dy; k++) {
                    locations[j][k]++;			//increment number of claims over entire area
                }
            }
        }
        
		//check all claims to see if they have been covered
        for(int i = 0; i < 1373; i++) {
            int x = coordss[i].a;
            int y = coordss[i].b;
            int dx = dimensionss[i].a;
            int dy = dimensionss[i].b;
            
            boolean complete = true;		//status of claim
            
            for(int j = x; j < x + dx; j++) {		//check entire area
                for(int k = y; k < y + dy; k++) {
                    if(locations[j][k] != 1) {		//if any square has been overlapped
                        complete = false;
                        break;
                    }
                }
                if(!complete) {
                    break;
                }
            }
            
            if(complete) {		//claim is untouched
                save = i;
                break;
            }
        }
        System.out.println(save + 1);		//claims are 1-indexed
    }
}

class Pair {
    int a, b;
    
    Pair(int a, int b) {
        this.a = a;
        this.b = b;
    }
}