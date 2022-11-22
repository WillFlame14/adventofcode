package testing2;

import java.util.*;
import java.io.*;

public class Testing2 {

    public static void main(String[] args)throws IOException {

        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        
        ArrayList<int[][]> image = new ArrayList<>();
        
        String s = br.readLine();
        
        int pointer = 0, lowest = 6*250, save = -1;
        
        while(pointer != s.length()) {
            int[][] layer = new int[6][25];
            int zeroes = 0, ones = 0, twos = 0;
            for(int row = 0; row < 6; row++) {
                for(int col = 0; col < 25; col++) {
                    layer[row][col] = Integer.parseInt(s.charAt(pointer) + "");
                    if(layer[row][col] == 0) {
                        zeroes++;
                    }
                    if(layer[row][col] == 1) {
                        ones++;
                    }
                    if(layer[row][col] == 2) {
                        twos++;
                    }
                    pointer++;
                }
            }
            if(zeroes < lowest) {
                lowest = zeroes;
                save = ones * twos;
            }
            image.add(layer);
        }
        //System.out.println(save);     //for part 1
        
        int[][] view = new int[6][25];
        for(int i = 0; i < 6; i++) {
            Arrays.fill(view[i], 2);        //fill entire array with transparent
        }
        
        for(int[][] layer : image) {
            for(int row = 0; row < 6; row++) {
                for(int col = 0; col < 25; col++) {
                    if(view[row][col] == 2) {
                        view[row][col] = layer[row][col];
                    }
                }
            }
        }
        for (int row = 0; row < 6; row++) {
            for (int col = 0; col < 25; col++) {
                if (view[row][col] == 1) {
                    System.out.print("0");
                }
                else {
                    System.out.print(" ");
                }
            }
            System.out.println();
        }
    }    
}
   
