import java.io.*;
import java.util.*;

//Part 1 was extremely easy, no code

public class AdventOfCode2018_8_2 {
    static Scanner sc = new Scanner(System.in);
    
    public static void main(String[] args) {
        System.out.println(generateNode());
    }
    
    private static int generateNode() {
        int numchilds = sc.nextInt();
        int[] metadata = new int[sc.nextInt()];
        int[] childvalues = new int[numchilds];
        
        for(int i = 0; i < numchilds; i++) {
            childvalues[i] = generateNode();
        }
        
        int sum = 0;
        
        for (int i = 0; i < metadata.length; i++) {
            metadata[i] = sc.nextInt();
            
            if(numchilds == 0) {
                sum += metadata[i];
            }
            else {
                try {
                    sum += childvalues[metadata[i] - 1];
                }
                catch (Exception e){ 
                }
            }
        }
        return sum;
    }
}