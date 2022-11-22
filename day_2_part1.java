import java.io.*;
import java.util.*;

public class AdventOfCode2018_2_1 {
    public static void main(String[] args)throws IOException {
        
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        
        int total2 = 0;		//total ids that have 2 of the same letter
        int total3 = 0;		//total ids that have 3 
        
        for(int i = 0; i < 250; i++) {
            String s = br.readLine();
            int[] letters = new int[26];		//array of letter frequencies
            Arrays.fill(letters, 0);
			
            for(int j = 0; j < s.length(); j++) {		//increment array element for each letter
                int c = s.charAt(j);
                letters[c - 97]++;
            }
            
            boolean two = false;		//flags so one id doesn't count more than once
            boolean three = false;
            for(int j = 0; j < 26; j++) {		
                if(letters[j] == 2 && !two) {			//haven't already found 2 occurences of a letter
                    total2++;
                    two = true;
                }
                else if(letters[j] == 3 && !three) {	//haven't already found 3 occurences of a letter
                    total3++;
                    three = true;
                }
                if(two && three) {		//no need to continue checking after both have been found
                    break;
                }
            }
        }
        System.out.println(total2 * total3);
    }
}