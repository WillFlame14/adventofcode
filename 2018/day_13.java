package randomstufftesting;

import java.util.*;
import java.io.*;

public class AdventOfCode2018_13 {
    
    static char[][] map;
    static char[][] dynamic;
    static ArrayList<Minecart> carts;
    static HashSet<Integer> completed;
    
    public static void main(String[] args)throws IOException {       
        
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        
        map = new char[150][150];
        dynamic = new char[150][150];
        carts = new ArrayList<>(10);
        
        int counter = 0;
        
        for(int i = 0; i < 150; i++) {
            String s = br.readLine();
            
            for(int j = 0; j < 150; j++) {
                if(s.charAt(j) == 'v' || s.charAt(j) == '^') {
                    map[i][j] = '|';
                    dynamic[i][j] = (char)(counter + 65);
                    carts.add(new Minecart(new Pair(i, j), (char)(counter + 65), (s.charAt(j)=='v')?Direction.DOWN:Direction.UP));
                    counter++;
                }
                else if(s.charAt(j) == '<' || s.charAt(j) == '>') {
                    map[i][j] = '-';
                    dynamic[i][j] = (char)(counter + 65);
                    carts.add(new Minecart(new Pair(i, j), (char)(counter + 65), (s.charAt(j)=='<')?Direction.LEFT:Direction.RIGHT));
                    counter++;
                }
                else {
                    map[i][j] = s.charAt(j);
                    dynamic[i][j] = s.charAt(j);
                }
            }
        }
        
        boolean escape = false;
        Pair collision = new Pair(-1, -1);
        completed = new HashSet<>();
        
        while(!escape) {
            HashSet<Integer> complete = new HashSet<>();
            for(int i = 0; i < 150; i++) {
                if(escape) {
                    break;
                }
                for(int j = 0; j < 150; j++) {
                    if("ABCDEFGHIJKLMNOPQRSTUVWXYZ".contains(dynamic[i][j] + "")) {
                        int minecart = (int)dynamic[i][j] - 65;
                        if(complete.contains(minecart)) {
                            continue;
                        }
//                        System.out.println(minecart);
                        carts.get(minecart).update();
//                        System.out.println(carts.get(minecart).position.a + " " + carts.get(minecart).position.b + " " + carts.get(minecart).direction + " " + minecart);
                        
                        
                        complete.add(minecart);
                        collision = checkCollision();
                        if(collision.a != -1 && completed.size() > 15) {
                            escape = true;
                            break;
                        }
                    }
                }
            }
//            System.out.println("advanced-----------");
        }
        carts.get(13).update();
        System.out.println(carts.get(13).position.a + " " + carts.get(13).position.b);
        System.out.println(collision.a + " " + collision.b);
    }
    
    public static Pair checkCollision() {
        for(int i = 0; i < carts.size(); i++) {
            for(int j = i + 1; j < carts.size(); j++) {
                if((carts.get(i).position.a == carts.get(j).position.a) && (carts.get(i).position.b == carts.get(j).position.b)) {
                    if(completed.contains(i) || completed.contains(j)) {
                        continue;
                    }
                    System.out.println(i + " " +  j + "collided");
                    completed.add(i);
                    completed.add(j);
                    dynamic[carts.get(i).position.a][carts.get(i).position.b] = map[carts.get(i).position.a][carts.get(i).position.b];
                    return carts.get(i).position;
                }
            }
        }
        return new Pair(-1, -1);
    }
}

class Minecart{
    Pair position;
    char id;
    int lastturn;
    Direction direction;
    
    public Minecart(Pair pos, char id, Direction dir) {
        position = pos;
        this.id = id;
        direction = dir;
        lastturn = 1;       //-1 is left, 0 is straight, 1 is right
    }
    
    public void update() {
        char next = ' ';
        RandomStuffTesting.dynamic[position.a][position.b] = RandomStuffTesting.map[position.a][position.b];        //overwrite old position
        switch(direction) {     
            case UP:
                next = RandomStuffTesting.map[position.a - 1][position.b];
                position.a -= 1;
                break;
            case DOWN:
                next = RandomStuffTesting.map[position.a + 1][position.b];
                position.a += 1;
                break;
            case LEFT:
                next = RandomStuffTesting.map[position.a][position.b - 1];
                position.b -= 1;
                break;
            case RIGHT:
                next = RandomStuffTesting.map[position.a][position.b + 1];
                position.b += 1;
                break;    
        }
//        System.out.println("next is " + next);
        RandomStuffTesting.dynamic[position.a][position.b] = (id + "").charAt(0);        //write new position
        
        switch(next) {
            case '/':
                switch(direction){
                    case UP:
                        direction = Direction.RIGHT;
                        break;
                    case DOWN:
                        direction = Direction.LEFT;
                        break;
                    case LEFT:
                        direction = Direction.DOWN;
                        break;
                    case RIGHT:
                        direction = Direction.UP;
                        break;
                }
                break;
            case '\\':
                switch(direction){
                    case UP:
                        direction = Direction.LEFT;
                        break;
                    case DOWN:
                        direction = Direction.RIGHT;
                        break;
                    case LEFT:
                        direction = Direction.UP;
                        break;
                    case RIGHT:
                        direction = Direction.DOWN;
                        break;
                }  
                break;
            case '+':
                switch(lastturn){       
                    case 1:     //last was right, now turn left
                        direction = direction.turnLeft();
                        lastturn = -1;       //just turned left
                        break;
                    case 0:     //last was straight, now turn right
                        direction = direction.turnRight();
                        lastturn = 1;       //just turned right
                        break;
                    case -1:    //last was left, now go straight
                        lastturn = 0;       //just went straight
                        break;
                }   
                break;
        }
    }
}

enum Direction {
    LEFT, RIGHT, DOWN, UP;
    
    public Direction turnLeft() {
        switch(this) {
            case LEFT:
                return DOWN;
            case DOWN:
                return RIGHT;
            case RIGHT:
                return UP;
            case UP:
                return LEFT;    
        }
        System.out.println("oops. turnLeft error");
        return DOWN;
    }
    
    public Direction turnRight() {
        switch(this) {
            case LEFT:
                return UP;
            case DOWN:
                return LEFT;
            case RIGHT:
                return DOWN;
            case UP:
                return RIGHT;    
        }
        System.out.println("oops. turnRight error");
        return DOWN;
    }
}

class Pair {
    int a, b;
    
    public Pair(int a, int b) {
        this.a = a;
        this.b = b;
    }
}

class Quad {
    int a, b, c, d;
    
    public Quad(int a, int b, int c, int d) {
        this.a = a;
        this.b = b;
        this.c = c;
        this.d = d;
    }
}