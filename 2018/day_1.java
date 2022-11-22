import java.io.*;
import java.util.*;

public class AdventOfCode2018_1_2 {
	
    public static void main(String[] args)throws IOException {
        
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        
        HashSet<Integer> hs = new HashSet<>();		//set containing all frequencies
        int[] list = new int[1033];					//list of frequency changes
        int total = 0;								//current frequency
        boolean found = false;
        
		//go through frequency changes the first time
        for(int i = 0; i < 1033; i++) {
            int num = Integer.parseInt(br.readLine());
            list[i] = num;
            total += num;
            if(hs.contains(total)) {
                System.out.println("duplicate is " + total);		//possibility that duplicate is found in first go-through
                break;		//for part 1, remove this break and output total after this loop
            }
            else {
                hs.add(total);
                //System.out.println("added " + total);
            }
        }
        
		//continue going through frequency changes until duplicate is found
        while(!found) {
            for(int i = 0; i < 1033; i++) {
                int num = list[i];
                total += num;
                if(hs.contains(total)) {
                    System.out.println("duplicate is " + total);
                    found = true;
                    break;
                }
                else {
                    hs.add(total);
                    //System.out.println("added " + total);
                }
            }
        }
        System.out.println("total is " + total);
    }
}