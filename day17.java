package testing4;

import java.util.*;
import java.io.*;

public class Testing4 {
    
    static long[] orig_array;
    static long[] orig_array2;
    static int array_size;
    static boolean exit = false;
    static ArrayList<ArrayList<Character>> tiles;
    static Machine machine;
    static int DIMENSION = 0, row = 1, col = 0;

    public static void main(String[] args)throws IOException {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        
        String input = br.readLine();       //add extra space in memory
        for(int i = 0; i < 3000; i++) {
            input += ",0";
        }
        
        String[] parts = input.split(",");
        array_size = parts.length;
        orig_array = new long[array_size];
        orig_array2 = new long[array_size];
        
        for(int i = 0; i < parts.length; i++) {
            orig_array[i] = Long.parseLong(parts[i]);
        }
        orig_array2 = Arrays.copyOf(orig_array, orig_array.length);
        tiles = new ArrayList<>();
        
        Pair position = new Pair(-1, -1);
        Direction dir = Direction.NA;
        machine = new Machine(orig_array, 0, 0, Direction.UP);
        
        ArrayList<Character> this_row = new ArrayList<>(50);
        while(!exit) {
            char result = (char)run(new ArrayList<>(), machine);
            if(exit) {
                break;
            }
            if(result == 10) {
                tiles.add((ArrayList<Character>)this_row.clone());
                this_row.clear();
            }
            else {
                if(result != '.' && result != '#') {
                    position.a = tiles.size();
                    position.b = this_row.size();
                    switch (result) {
                        case '^':
                            dir = Direction.UP;
                            break;
                        case '<':
                            dir = Direction.LEFT;
                            break;
                        case '>':
                            dir = Direction.RIGHT;
                            break;
                        case 'v':
                            dir = Direction.DOWN;
                            break;
                    }
                }
                this_row.add(result);
            }
        }
        tiles.remove(tiles.size() - 1);
        print_board();
        
        HashSet<Integer> intersections = new HashSet<>();
        long total = 0;       //part 1
        
        for(int i = 1; i < tiles.size() - 1; i++) {
            for(int j = 1; j < tiles.get(0).size() - 1; j++) {
                if(tiles.get(i).get(j) == '#') {
                    if(tiles.get(i - 1).get(j) == '#' && tiles.get(i + 1).get(j) == '#') {
                        if(tiles.get(i).get(j - 1) == '#' && tiles.get(i).get(j + 1) == '#') {
                            intersections.add(i * 100 + j);
                            total += i * j;
                        }
                    }
                }
            }
        }
        System.out.println(total);

        ArrayList<Character> instructions = new ArrayList<>(20);
        Direction to_move = can_move(position, dir);
        
        while(to_move != Direction.NA) {
            if(to_move == Direction.MULTIPLE || to_move == dir) {     //keep moving in same direction
                instructions.set(instructions.size() - 1, (char)(instructions.get(instructions.size() - 1) + 1));
            }
            else {
                if(to_move == dir.rotateCCW90()) {
                    instructions.add('L');
                }
                else {
                    if(to_move != dir.rotateCW90()) {
                        System.out.println("ohgodwhy");
                    }
                    instructions.add('R');
                }
                dir = to_move;
                instructions.add('1');
            }
            switch(dir) {
                case LEFT:
                    position.b--;
                    break;
                case RIGHT:
                    position.b++;
                    break;
                case UP:
                    position.a--;
                    break;
                case DOWN:
                    position.a++;
                    break;
            }
            if(intersections.contains(position.a * 100 + position.b)) {
                intersections.remove(position.a * 100 + position.b);
            }
            else {
                tiles.get(position.a).set(position.b, '%');
            }
            to_move = can_move(position, dir);
        }
        
        HashMap<String, Integer> occurrences = new HashMap<>();
        
        for(int subseq = 3; subseq < 11; subseq++) {        //subsequence cannot be longer than 10
           char[] pattern = new char[subseq];
            for(int j = 0; j < instructions.size() - (subseq - 1); j++) {
                String str = "";
                for(int m = 0; m < subseq; m++) {
                    pattern[m] = instructions.get(j + m);
                    str += instructions.get(j + m) + " ";
                }
                if(occurrences.containsKey(str)) {
                    continue;
                }
                int times = 1;
                for(int k = j + subseq; k < instructions.size() - (subseq - 1); k++) {
                    boolean valid = true;
                    for(int m = 0; m < subseq; m++) {
                        if(instructions.get(k + m) != pattern[m]) {
                            valid = false;
                            break;
                        }
                    }
                    if(valid) {
                        times++;
                        k += subseq - 1;
                    }
                }
                if(times > 2) {
                    occurrences.put(str, times);
                }
            }
        }
        
        Set<String> patterns = occurrences.keySet();
        String inst = "";
        for(Character c: instructions) {
            inst += c + " ";
        }
        
        int shortest = 1000;
        String routine = "";
        String[] functions = new String[3];
        
        for(String s: patterns) {
            for(String t: patterns) {
                if(t.equals(s)) {
                    continue;
                }
                for(String u: patterns) {
                    if(u.equals(s) || u.equals(t)) {
                        continue;
                    }
                    String test = inst + "";      //generate new string
                    test = test.replace(s, "A").replace(t, "B").replace(u, "C");
                    if(test.length() < shortest) {
                        functions[0] = s;
                        functions[1] = t;
                        functions[2] = u;
                        routine = test;
                        shortest = test.length();
                    }
                }
            }
        }
        ArrayList<Integer> inputs = new ArrayList<>();
        for(int i = 0; i < routine.length(); i++) {
            inputs.add((int)routine.charAt(i));
            if(i != routine.length() - 1) {
                inputs.add((int)',');
            }
            else {
                inputs.add(10);
            }
        }
        for(int j = 0; j < 3; j++) {
            String[] function_parts = functions[j].split(" ");
            for(int i = 0; i < function_parts.length; i++) {
                if(function_parts[i].equals("")) {
                    continue;
                }
                char c = function_parts[i].charAt(0);
                if(c >= '0' && c <= '9') {      //number from 0-9
                    inputs.add((int)c);
                }
                else if(c >= ':' && c <= 'K') {     //number from 10+
                    int num = c - '0';
                    inputs.add((int)((num / 10) + "").charAt(0));
                    inputs.add((int)((num % 10) + "").charAt(0));
                }
                else {                      //L or R
                    inputs.add((int)c);
                }
                if(i != function_parts.length - 1) {
                    inputs.add((int)',');
                }
                else {
                    inputs.add(10);
                }
            }
        }
        inputs.add((int)'n');       //turn on camera view?
        inputs.add(10);
        
        orig_array2[0] = 2;      //force wake up
        exit = false;           //reset state
        Machine machine2 = new Machine(orig_array2, 0, 0, Direction.UP);
        while(!exit) {
            char result = (char)run(inputs, machine2);
            System.out.print(result);
        }       //the output for final answer is coded into a case in opcode 4 (large, non-ASCII value)
    } 
    
    public static Direction can_move(Pair p, Direction current) {
        Direction dir = Direction.NA;
        //System.out.println(p.a + " " + p.b);
        //left wall
        if(p.b != 0 && tiles.get(p.a).get(p.b - 1) == '#' && current.opposite() != Direction.LEFT) {
            if(dir == Direction.NA) {
                dir = Direction.LEFT;
            }
            else {
                return Direction.MULTIPLE;
            }
        }
        
        //right wall
        if(p.b != tiles.get(0).size() - 1 && tiles.get(p.a).get(p.b + 1) == '#' && current.opposite() != Direction.RIGHT) {
            if(dir == Direction.NA) {
                dir = Direction.RIGHT;
            }
            else {
                return Direction.MULTIPLE;
            }
        }
        
        //top wall
        if(p.a != 0 && tiles.get(p.a - 1).get(p.b) == '#' && current.opposite() != Direction.UP) {
            if(dir == Direction.NA) {
                dir = Direction.UP;
            }
            else {
                return Direction.MULTIPLE;
            }
        }
        
        //bottom wall
        if(p.a != tiles.size() - 1 && tiles.get(p.a + 1).get(p.b) == '#' && current.opposite() != Direction.DOWN) {
            if(dir == Direction.NA) {
                dir = Direction.DOWN;
            }
            else {
                return Direction.MULTIPLE;
            }
        }
        return dir;
    }
    
    public static long run(ArrayList<Integer> inputs, Machine machine) {
        long save = -1;
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
                    if (mode_a == 0) {
                        machine.values[(int)a] = inputs.get(machine.inputs_given);     
                    }
                    else if(mode_a == 2) {
                        machine.values[(int)a + machine.offset] = inputs.get(machine.inputs_given);
                    }
                    System.out.println(inputs.get(machine.inputs_given) + ", " + machine.inputs_given);
                    machine.inputs_given++;
                    break;
                case 4:
                    if (mode_a == 0) {      //both can be in position mode
                        a = machine.values[(int)a];
                    }   
                    else if(mode_a == 2) {
                        a = machine.values[(int)a + machine.offset];
                    }
                    if(a > 127) {
                        System.out.println(a);      //large, non-ASCII value
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
        for (int i = 0; i < tiles.size(); i++) {
            for (int j = 0; j < tiles.get(0).size(); j++) {
                System.out.print(tiles.get(i).get(j));
            }
            System.out.println();
        }
    }
}

class Machine {
    long[] values;
    int pointer, offset, inputs_given, outputs_given;
    Direction direction;
    
    public Machine(long[] values, int pointer, int offset, Direction direction) {
        this.values = values;
        this.pointer = pointer;
        this.offset = offset;
        inputs_given = 0;
        outputs_given = 0;
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
    NA, LEFT, UP, DOWN, RIGHT, MULTIPLE;
    
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
        return NA;
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
        return NA;
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
        return NA;
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
        return NA;
    }
}