package testing2;

import java.util.*;
import java.io.*;

public class Testing2 {
    
    static int[] orig_array;
    static int array_size;
    static final int NUM_AMPLIFIERS = 5;
    static final boolean PART_ONE = false;
    static boolean exit = false;

    public static void main(String[] args)throws IOException {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        
        String input = br.readLine();       //add extra space in memory
        for(int i = 0; i < 10; i++) {
            input += ",0";
        }
        
        String[] parts = input.split(",");
        array_size = parts.length;
        orig_array = new int[array_size];
        
        for(int i = 0; i < parts.length; i++) {
            orig_array[i] = Integer.parseInt(parts[i]);
        }
        
        ArrayList<int[]> permutations = new ArrayList<>();
        
        for(int a = 5; a < 10; a++) {
            for(int b = 5; b < 10; b++) {
                if(b == a)
                    continue;
                for(int c = 5; c < 10; c++) {
                    if(c == b || c == a)
                        continue;
                    for(int d = 5; d < 10; d++) {
                        if(d == c || d == b || d == a)
                            continue;
                        for(int e = 5; e < 10; e++) {
                            if(e == d || e == c || e == b || e == a)
                                continue;
                            
                            int[] values = new int[5];
                            values[0] = a;
                            values[1] = b;
                            values[2] = c;
                            values[3] = d;
                            values[4] = e;
                            permutations.add(values);
                        }
                    }
                }
            }
        }
        
        int highest = 0;
                            
        for(int[] values: permutations) {
            Amplifier[] amplifiers = new Amplifier[NUM_AMPLIFIERS];
            for(int i = 0; i < NUM_AMPLIFIERS; i++) {       //add n copies of original array
                Amplifier machine = new Amplifier(Arrays.copyOf(orig_array, array_size), 0);
                amplifiers[i] = machine;
            }
            
            int result = 0;     //the initial input value
            exit = false;
            
            if(PART_ONE) {
                for(int i = 0; i < NUM_AMPLIFIERS; i++) {
                    ArrayList<Integer> inputs = new ArrayList<>();
                    inputs.add(values[i] - 5);      //for part 1, use nums 0-4
                    inputs.add(result);
                    result = run(inputs, amplifiers[i]);
                }
            }
            else {
                for(int i = 0; i < NUM_AMPLIFIERS; i++) {
                    ArrayList<Integer> inputs = new ArrayList<>();
                    inputs.add(values[i]);
                    inputs.add(result);
                    result = run(inputs, amplifiers[i]);
                }
                while(!exit) {
                    for(int i = 0; i < NUM_AMPLIFIERS; i++) {
                        ArrayList<Integer> inputs = new ArrayList<>();
                        inputs.add(result);
                        result = run(inputs, amplifiers[i]);
                    }
                }
            }
            if(result > highest) {
                highest = result;
            }
        }  
        System.out.println(highest);
    } 
    
    public static int run(ArrayList<Integer> inputs, Amplifier amplifier) {
        int save = -1, inputs_given = 0;
        boolean finish = false;
        while (!finish) {
            int mode = amplifier.values[amplifier.pointer], a, b, c;
            amplifier.pointer++;
            
            int opcode = mode % 100, mode_a, mode_b, mode_c;
            mode /= 100;
            mode_a = mode % 10;
            mode /= 10;
            mode_b = mode % 10;
            mode /= 10;
            mode_c = mode % 10;
            
            a = amplifier.values[amplifier.pointer];
            amplifier.pointer++;
            switch (opcode) {
                case 1:     //takes 3 parameters
                case 2:
                case 7:
                case 8:
                    b = amplifier.values[amplifier.pointer];
                    amplifier.pointer++;
                    c = amplifier.values[amplifier.pointer];
                    amplifier.pointer++;
                    
                    if (mode_a == 0) {     //c cannot be immediate mode because problem statement is bad
                        a = amplifier.values[a];
                    }
                    if (mode_b == 0) {
                        b = amplifier.values[b];
                    }   
                    
                    switch (opcode) {
                        case 1:
                            amplifier.values[c] = a + b;
                            break;
                        case 2:
                            amplifier.values[c] = a * b;
                            break;
                        case 7:
                            amplifier.values[c] = (a < b) ? 1 : 0;
                            break;
                        case 8:
                            amplifier.values[c] = (a == b) ? 1 : 0;
                            break;
                    }
                    break;
                case 3:
                    if (inputs_given < inputs.size()) {
                        amplifier.values[a] = inputs.get(inputs_given);
                        inputs_given++;
                    } else {        //waiting for input
                        amplifier.pointer -= 2;      //when loading, ask for this opcode again
                        finish = true;
                    }
                    break;
                case 4:
                    if (save != -1) {       //input already ready to send out, move on
                        amplifier.pointer -= 2;       //when loading, ask for this opcode again
                        finish = true;
                        break;
                    }
                    save = amplifier.values[a];
                    break;
                case 5:
                case 6:
                    b = amplifier.values[amplifier.pointer];
                    amplifier.pointer++;
                    
                    if (mode_a == 0) {      //both can be in position mode
                        a = amplifier.values[a];
                    }   
                    if (mode_b == 0) {
                        b = amplifier.values[b];
                    }   
                    
                    if ((a != 0 && opcode == 5) || (a == 0 && opcode == 6)) {
                        amplifier.pointer = b;
                    }   
                    break;
                case 99:
                    System.out.println("STOP");
                    exit = true;
                    finish = true;
                    break;
                default:
                    System.out.println("Unknown opcode " + opcode + " encountered.");
                    exit = true;
                    finish = true;
                    break;
            }
        }
        return save;
    }
}

class Amplifier {
    int[] values;
    int pointer;
    
    public Amplifier(int[] values, int pointer) {
        this.values = values;
        this.pointer = pointer;
    }
}