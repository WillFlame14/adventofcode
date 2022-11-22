package testing4;

import java.util.*;
import java.io.*;

public class Testing4 {
    
    static long[] orig_array;
    static int array_size;
    static boolean exit = false;
    static char[][] tiles;
    static Machine machine;
    static final int DIMENSION = 50;

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
        
        tiles = new char[DIMENSION][DIMENSION];
        for(int i = 0; i < DIMENSION; i++) {
            Arrays.fill(tiles[i], ' ');     //fill entire map with empty space
        }
        tiles[DIMENSION/2][DIMENSION/2] = 'x';      //starting location
        
        machine = new Machine(orig_array, 0, 0, Direction.UP);
        System.out.println(find(new Pair(DIMENSION/2, DIMENSION/2), 0));        //start recursion with initial position, path length = 0
        print_board();
        
        int counter = 0;
        tiles[DIMENSION/2][DIMENSION/2] = '.';      //change the starting tile back to just empty space
        
        while(!isFilledOxygen()) {
            spreadOxygen();
            counter++;
        }
        print_board();
        System.out.println(counter);
    } 
    
    public static void spreadOxygen() {
        for(int i = 0; i < DIMENSION; i++) {
            for(int j = 0; j < DIMENSION; j++) {
                if(tiles[i][j] == '@') {
                    if(tiles[i + 1][j] == '.') {
                        tiles[i + 1][j] = '-';
                    }
                    if(tiles[i - 1][j] == '.') {
                        tiles[i - 1][j] = '-';
                    }
                    if(tiles[i][j + 1] == '.') {
                        tiles[i][j + 1] = '-';
                    }
                    if(tiles[i][j - 1] == '.') {
                        tiles[i][j - 1] = '-';
                    }
                }
            }
        }
        for(int i = 0; i < DIMENSION; i++) {
            for(int j = 0; j < DIMENSION; j++) {
                if(tiles[i][j] == '-') {
                    tiles[i][j] = '@';
                }
            }
        }
    }
    
    public static boolean isFilledOxygen() {
        for(int i = 0; i < DIMENSION; i++) {
            for(int j = 0; j < DIMENSION; j++) {
                if(tiles[i][j] == '.') {
                    return false;
                }
            }
        }
        return true;
    }
    
    public static int find(Pair position, int path) {
        Direction dir = Direction.UP;
        int save = -1;
        for(int i = 0; i < 4; i++) {        //rotate 4 times
            Pair newpos = position.clone();
            switch (dir.toInt()) {
                case 1:     //north
                    newpos.a -= 1;
                    break;
                case 2:     //south
                    newpos.a += 1;
                    break;
                case 3:     //west
                    newpos.b -= 1;
                    break;
                case 4:     //east
                    newpos.b += 1;
                    break;
            }
            if(tiles[newpos.a][newpos.b] == ' ') {      //unexplored
                ArrayList<Integer> inputs = new ArrayList<>();
                inputs.add(dir.toInt());
                int output = (int)run(inputs, machine);
                switch(output) {
                    case 0:
                        tiles[newpos.a][newpos.b] = '#';
                        break;
                    case 1:
                        tiles[newpos.a][newpos.b] = '.';
                        save = find(newpos, path + 1);
                        if(save != -1) {        //save can be -1 if recursion is satisfied by dead end, so need to check that oxygen system was found
                            return save;
                        }
                        break;
                    case 2:
                        tiles[newpos.a][newpos.b] = '@';
                        find(newpos, path + 1);
                        //return path + 1;      //uncomment this for part 1 (will often get the correct part 2 solution as well, but not advised)
                        break;
                }
                if (output != 0) {
                    inputs.clear();
                    inputs.add(dir.opposite().toInt());     //undo the movement so the machine can reset
                    run(inputs, machine);
                }
            }
            dir = dir.rotateCW90();     //rotate direction
        }
        return save;
    }
    
    public static long run(ArrayList<Integer> inputs, Machine machine) {
        long save = -1;
//        int inputs_given = 0;
//        int outputs_given = 0;
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
                        machine.values[(int)a] = inputs.get(0);     //only 1 input
                    }
                    else if(mode_a == 2) {
                        machine.values[(int)a + machine.offset] = inputs.get(0);
                    }
                    break;
                case 4:
                    if (mode_a == 0) {      //both can be in position mode
                        a = machine.values[(int)a];
                    }   
                    else if(mode_a == 2) {
                        a = machine.values[(int)a + machine.offset];
                    }
                    return a;
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
    
    public static void print_board() {
        for (int i = 0; i < DIMENSION; i++) {
            for (int j = 0; j < DIMENSION; j++) {
                System.out.print(tiles[i][j]);
            }
            System.out.println();
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
    
    public Pair clone() {
        return new Pair(a, b);
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
    
    public Direction opposite() {
        switch(this) {
            case LEFT:
                return RIGHT;
            case RIGHT:
                return LEFT;
            case UP:
                return DOWN;
            case DOWN:
                return UP;
        }
        return UP;
    }
    
    public int toInt() {
        switch(this) {
            case LEFT:
                return 3;
            case UP:
                return 1;
            case RIGHT:
                return 4;
            case DOWN:
                return 2;
        }
        return -1;
    }
    
    public static Direction toDirection(int dir) {
        switch(dir) {
            case 1:
                return UP;
            case 2:
                return DOWN;
            case 3: 
                return LEFT;
            case 4:
                return RIGHT;
        }
        return UP;
    }
}