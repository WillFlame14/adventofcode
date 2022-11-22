package testing2;

import java.util.*;
import java.io.*;

public class Testing2 {
    
    static long[] orig_array;
    static int array_size;
    static boolean exit = false;

    public static void main(String[] args)throws IOException {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        
        String input = br.readLine();       //add extra space in memory
        for(int i = 0; i < 250; i++) {
            input += ",0";
        }
        
        String[] parts = input.split(",");
        array_size = parts.length;
        orig_array = new long[array_size];
        
        for(int i = 0; i < parts.length; i++) {
            orig_array[i] = Long.parseLong(parts[i]);
        }
        ArrayList<Integer> inputs = new ArrayList<>();
        inputs.add(2);      //change to inputs.add(1); for part 1
        System.out.println(run(inputs, new Amplifier(orig_array, 0, 0)));
    } 
    
    public static long run(ArrayList<Integer> inputs, Amplifier amplifier) {
        long save = -1;
        int inputs_given = 0;
        boolean finish = false;
        while (!finish) {
            long mode = amplifier.values[amplifier.pointer];
            long a, b, c;
            amplifier.pointer++;
            
            int opcode = (int)(mode % 100), mode_a, mode_b, mode_c;
            mode /= 100;
            mode_a = (int)mode % 10;
            mode /= 10;
            mode_b = (int)mode % 10;
            mode /= 10;
            mode_c = (int)mode % 10;
            
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
                    
                    if (mode_a == 0) {     
                        a = amplifier.values[(int)a];
                    }
                    else if(mode_a == 2) {
                        a = amplifier.values[(int)a + amplifier.offset];
                    }
                    if (mode_b == 0) {
                        b = amplifier.values[(int)b];
                    }
                    else if(mode_b == 2) {
                        b = amplifier.values[(int)b + amplifier.offset];
                    }
                    if(mode_c == 2) {
                        c += amplifier.offset;
                    }
                    
                    switch (opcode) {
                        case 1:
                            amplifier.values[(int)c] = a + b;
                            break;
                        case 2:
                            amplifier.values[(int)c] = a * b;
                            break;
                        case 7:
                            amplifier.values[(int)c] = (a < b) ? 1 : 0;
                            break;
                        case 8:
                            amplifier.values[(int)c] = (a == b) ? 1 : 0;
                            break;
                    }
                    break;
                case 3:
                    if (inputs_given < inputs.size()) {
                        if(mode_a == 0) {
                            amplifier.values[(int)a] = inputs.get(inputs_given);
                        }
                        else if(mode_a == 2) {
                            amplifier.values[(int)a + amplifier.offset] = inputs.get(inputs_given);
                        }
                        inputs_given++;
                    } else {        //waiting for input
                        amplifier.pointer -= 2;      //when loading, ask for this opcode again
                        finish = true;
                    }
                    break;
                case 4:
//                    if (save != -1) {       //input already ready to send out, move on
//                        amplifier.pointer -= 2;       //when loading, ask for this opcode again
//                        finish = true;
//                        break;
//                    }
//                    save = amplifier.values[(int)a];
                    if (mode_a == 0) {      //both can be in position mode
                        a = amplifier.values[(int)a];
                    }   
                    else if(mode_a == 2) {
                        a = amplifier.values[(int)a + amplifier.offset];
                    }
                    System.out.print(a + " ");
                    break;
                case 5:
                case 6:
                    b = amplifier.values[amplifier.pointer];
                    amplifier.pointer++;
                    
                    if (mode_a == 0) {      //both can be in position mode
                        a = amplifier.values[(int)a];
                    }   
                    else if(mode_a == 2) {
                        a = amplifier.values[(int)a + amplifier.offset];
                    }
                    if (mode_b == 0) {
                        b = amplifier.values[(int)b];
                    }
                    else if(mode_b == 2) {
                        b = amplifier.values[(int)b + amplifier.offset];
                    }
                    
                    if ((a != 0 && opcode == 5) || (a == 0 && opcode == 6)) {
                        amplifier.pointer = (int)b;
                    }   
                    break;
                case 9:
                    if (mode_a == 0) {
                        a = amplifier.values[(int)a];
                    }   
                    else if(mode_a == 2) {
                        a = amplifier.values[(int)a + amplifier.offset];
                    }
                    amplifier.offset += (int)a;
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
    long[] values;
    int pointer, offset;
    
    public Amplifier(long[] values, int pointer, int offset) {
        this.values = values;
        this.pointer = pointer;
        this.offset = offset;
    }
}