import java.io.*;
import java.util.*;

public class AdventOfCode2018_day2_2 {
    public static void main(String[] args)throws IOException {
        
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        
        String[] list = new String[250];		//list of all ids
        
        for(int i = 0; i < 250; i++) {
            list[i] = br.readLine();
        }
        
        for(int i = 0; i < 250; i++) {		//check all combinations of strings
            for(int j = i; j < 250; j++) {
                String s1 = list[i];
                String s2 = list[j];
                
                int counter = 0, index = -1;
                for(int k = 0; k < s1.length(); k++) {
                    if(s1.charAt(k) != s2.charAt(k)) {		//if letter is different
                        index = k;
                        counter++;
                    }
                    if(counter > 1) {		//more than 1 different letter
                        break;
                    }
                }
                if(counter == 1) {
                    System.out.println(s1.substring(0, index) + s1.substring(index + 1));		//print string without the different letter
                }
            }
        }
    }
}