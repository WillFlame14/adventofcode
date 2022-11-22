package testing2;

import java.util.*;
import java.io.*;

public class Testing2 {
    
    static long[] orig_array;
    static int array_size;
    static boolean exit = false;
    static int[][] grid;
    static boolean[][] visited;
    static Pair position;

    public static void main(String[] args)throws IOException {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        
        String input = br.readLine();       //add extra space in memory
        for(int i = 0; i < 1000; i++) {
            input += ",0";
        }
        
        String[] parts = input.split(",");
        array_size = parts.length;
        orig_array = new long[array_size];
        
        for(int i = 0; i < parts.length; i++) {
            orig_array[i] = Long.parseLong(parts[i]);
        }
        
        grid = new int[200][200];
        visited = new boolean[200][200];
        for(int i = 0; i < 200; i++) {
            Arrays.fill(grid[i], 0);
            Arrays.fill(visited[i], false);
        }
        
        position = new Pair(100, 100);
        visited[100][100] = true;
        
        grid[100][100] = 1;
        
        ArrayList<Integer> inputs = new ArrayList<>();      //inputs don't matter for today
        System.out.println(run(inputs, new Machine(orig_array, 0, 0, Direction.UP)));
        
    } 
    
    public static long run(ArrayList<Integer> inputs, Machine machine) {
        long save = -1, panels = 1;
        //int inputs_given = 0;
        int outputs_given = 0;
        boolean finish = false;
        while (!finish) {
            long mode = machine.values[machine.pointer];
            long a, b, c;
            machine.pointer++;
            
            int opcode = (int)(mode % 100), mode_a, mode_b, mode_c;
            mode /= 100;
            mode_a = (int)mode % 10;
            mode /= 10;
            mode_b = (int)mode % 10;
            mode /= 10;
            mode_c = (int)mode % 10;
            
            a = machine.values[machine.pointer];
            machine.pointer++;
            switch (opcode) {
                case 1:     //takes 3 parameters
                case 2:
                case 7:
                case 8:
                    b = machine.values[machine.pointer];
                    machine.pointer++;
                    c = machine.values[machine.pointer];
                    machine.pointer++;
                    
                    if (mode_a == 0) {     
                        a = machine.values[(int)a];
                    }
                    else if(mode_a == 2) {
                        a = machine.values[(int)a + machine.offset];
                    }
                    if (mode_b == 0) {
                        b = machine.values[(int)b];
                    }
                    else if(mode_b == 2) {
                        b = machine.values[(int)b + machine.offset];
                    }
                    if(mode_c == 2) {
                        c += machine.offset;
                    }
                    
                    switch (opcode) {
                        case 1:
                            machine.values[(int)c] = a + b;
                            break;
                        case 2:
                            machine.values[(int)c] = a * b;
                            break;
                        case 7:
                            machine.values[(int)c] = (a < b) ? 1 : 0;
                            break;
                        case 8:
                            machine.values[(int)c] = (a == b) ? 1 : 0;
                            break;
                    }
                    break;
                case 3:
//                    if (inputs_given < inputs.size()) {
//                        if(mode_a == 0) {
//                            machine.values[(int)a] = inputs.get(inputs_given);
//                        }
//                        else if(mode_a == 2) {
//                            machine.values[(int)a + machine.offset] = inputs.get(inputs_given);
//                        }
//                        inputs_given++;
//                    } else {        //waiting for input
//                        machine.pointer -= 2;      //when loading, ask for this opcode again
//                        finish = true;
//                    }
                    if (mode_a == 0) {
                        machine.values[(int)a] = grid[position.a][position.b];
                    }
                    else if(mode_a == 2) {
                        machine.values[(int)a + machine.offset] = grid[position.a][position.b];
                    }
                    break;
                case 4:
//                    if (save != -1) {       //input already ready to send out, move on
//                        machine.pointer -= 2;       //when loading, ask for this opcode again
//                        finish = true;
//                        break;
//                    }
//                    save = machine.values[(int)a];
                    if (mode_a == 0) {      //both can be in position mode
                        a = machine.values[(int)a];
                    }   
                    else if(mode_a == 2) {
                        a = machine.values[(int)a + machine.offset];
                    }
                    if(outputs_given == 0) {
                        grid[position.a][position.b] = (int)a;
                        //System.out.println(a);
                        outputs_given++;
                    }
                    else {
                        machine.direction = (a == 0)?machine.direction.rotateCCW90():machine.direction.rotateCW90();
                        adjust(machine.direction, position);
                        if(!visited[position.a][position.b]) {
                            visited[position.a][position.b] = true;
                            panels++;
                        }
                        outputs_given = 0;
                    }
                    break;
                case 5:
                case 6:
                    b = machine.values[machine.pointer];
                    machine.pointer++;
                    
                    if (mode_a == 0) {      //both can be in position mode
                        a = machine.values[(int)a];
                    }   
                    else if(mode_a == 2) {
                        a = machine.values[(int)a + machine.offset];
                    }
                    if (mode_b == 0) {
                        b = machine.values[(int)b];
                    }
                    else if(mode_b == 2) {
                        b = machine.values[(int)b + machine.offset];
                    }
                    
                    if ((a != 0 && opcode == 5) || (a == 0 && opcode == 6)) {
                        machine.pointer = (int)b;
                    }   
                    break;
                case 9:
                    if (mode_a == 0) {
                        a = machine.values[(int)a];
                    }   
                    else if(mode_a == 2) {
                        a = machine.values[(int)a + machine.offset];
                    }
                    machine.offset += (int)a;
                    break;
                case 99:
                    System.out.println("STOP");
                    System.out.println(panels);
                    for (int i = 0; i < 200; i++) {
                        for (int j = 0; j < 200; j++) {
                            System.out.print(grid[i][j] == 0 ? " " : "#");
                        }
                        System.out.println();
                    }
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
    
    public static void adjust(Direction dir, Pair p) {
        switch(dir) {
            case LEFT:
                p.a--;
                break;
            case RIGHT:
                p.a++;
                break;
            case UP:
                p.b++;
                break;
            case DOWN:
                p.b--;
                break;
        }
    }
}

class Machine {
    long[] values;
    int pointer, offset;
    Direction direction;
    
    public Machine(long[] values, int pointer, int offset, Direction direction) {
        this.values = values;
        this.pointer = pointer;
        this.offset = offset;
        this.direction = direction;
    }
}

class Pair {
    int a, b;
    
    public Pair(int a, int b) {
        this.a = a;
        this.b = b;
    }
}

enum Direction {
    LEFT, UP, DOWN, RIGHT;
    
    public Direction rotateCW90() {
        switch(this) {
            case LEFT:
                return UP;
            case UP:
                return RIGHT;
            case DOWN:
                return LEFT;
            case RIGHT:
                return DOWN;
        }
        return UP;
    }
    
    public Direction rotateCCW90() {
        switch(this) {
            case LEFT:
                return DOWN;
            case UP:
                return LEFT;
            case DOWN:
                return RIGHT;
            case RIGHT:
                return UP;
        }
        return UP;
    }
}