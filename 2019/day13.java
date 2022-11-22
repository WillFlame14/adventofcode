package testing2;

import java.util.*;
import java.io.*;

public class Testing2 {
    
    static long[] orig_array;
    static int array_size;
    static boolean exit = false;
    static int[][] tiles;

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
        
        tiles = new int[20][40];
        
        ArrayList<Integer> inputs = new ArrayList<>();      //inputs don't matter for today
        System.out.println(run(inputs, new Machine(orig_array, 0, 0, Direction.UP)));
    } 
    
    public static long run(ArrayList<Integer> inputs, Machine machine) {
        long save = -1, panels = 1, score = -1;
        //int inputs_given = 0;
        int outputs_given = 0, row = -1, col = -1, joystick = 0;
        Pair position = new Pair(-1, -1), lastball = new Pair(-1, -1), currentball = new Pair(-1, -1);
        Direction balldirection = Direction.LEFT;
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
                        machine.values[(int)a] = joystick;
                    }
                    else if(mode_a == 2) {
                        machine.values[(int)a + machine.offset] = joystick;
                    }
                    break;
                case 4:
                    if (mode_a == 0) {      //both can be in position mode
                        a = machine.values[(int)a];
                    }   
                    else if(mode_a == 2) {
                        a = machine.values[(int)a + machine.offset];
                    }
                    if(outputs_given == 0) {
                        col = (int)a;
                        outputs_given++;
                    }
                    else if(outputs_given == 1) {
                        row = (int)a;
                        outputs_given++;
                    }
                    else {
                        if(col == -1 && row == 0) {
                            score = (int)a;
                            System.out.println("SCORE: " + score);
                        }
                        else {
                            tiles[row][col] = (int)a;
                            if(a == 4) {
                                if(col == position.b) {
                                    joystick = (balldirection == Direction.RIGHT)?1:-1;
                                    if(row == position.a - 1) {
                                        joystick = 0;       //don't move when ball is about to bounce
                                    }
                                }
                                else {
                                    joystick = (col > position.b)?1:-1;
                                }
                                lastball = currentball.clone();
                                currentball = new Pair(row, col);
                                balldirection = (col > lastball.b)?Direction.RIGHT:Direction.LEFT;
                            }
                            if(a == 3) {
                                position.a = row;
                                position.b = col;
                            }
                            //if(score != -1)
                                //print_board();
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
        for (int i = 0; i < 20; i++) {
            for (int j = 0; j < 40; j++) {
                switch (tiles[i][j]) {
                    case 0:
                        System.out.print(" ");
                        break;
                    case 1:
                        System.out.print("#");
                        break;
                    case 2:
                        System.out.print("@");
                        break;
                    case 3:
                        System.out.print("=");
                        break;
                    case 4:
                        System.out.print("0");
                        break;
                }
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
}