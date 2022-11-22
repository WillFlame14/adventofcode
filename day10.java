package main;

import java.util.*;
import java.io.*;

public class Main {
    
    static final int DIMENSION = 27;
  
    public static void main(String[] args)throws IOException {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
      
        boolean[][] grid = new boolean[DIMENSION][DIMENSION];     //grid of asteroids
        ArrayList<Pair> asteroids = new ArrayList<>();
      
        for(int i = 0; i < DIMENSION; i++) {
            String s = br.readLine();
          
            for(int j = 0; j < s.length(); j++) {
                grid[i][j] = s.charAt(j) == '#';
                if(grid[i][j]) {
                    asteroids.add(new Pair(i, j));
                }
            }
        }
        
        int highest = 0, x = -1, y = -1;
      
        for(int i = 0; i < DIMENSION; i++) {
            for(int j = 0; j < DIMENSION; j++) {
                int total = 0;
                if (!grid[i][j]) {       //no asteroid
                    continue;
                }
                for (int m = 0; m < DIMENSION; m++) {
                    for (int n = 0; n < DIMENSION; n++) {
                        if(!grid[m][n]) {       //no asteroid
                            continue;
                        }
                        if(m == i && n == j) {
                            continue;
                        }
                        boolean visible = true;
                        for(Pair p: asteroids) {
                            if(m == p.a && n == p.b) {      //comparing some asteroid
                                continue;
                            }
                            if(i == p.a && j == p.b) {
                                continue;
                            }
                            if((m - i) / (double)(n - j) == (p.a - i) / (double)(p.b - j) && (new Pair(m, n).dist(new Pair(i, j)) > p.dist(new Pair(i, j)))) {
                                if((n - j) * (p.b - j) < 0) {      //opposite sides
                                    continue;
                                }
                                if((m - i) * (p.a - i) < 0) {      //opposite sides
                                    continue;
                                }
                                visible = false;
                                break;
                            }
                        }
                        if(visible) {
                            total++;
                        }
                    }
                }
                if(total > highest) {
                    highest = total;
                    x = i;
                    y = j;
                }
            }
        }
        System.out.println(highest + " " + x + " " + y);
        
        ArrayList<Asteroid> toRemove = new ArrayList<>(100);
        for (int m = 0; m < DIMENSION; m++) {
            for (int n = 0; n < DIMENSION; n++) {
                if (!grid[m][n]) {       //no asteroid
                    continue;
                }
                if (m == x && n == y) {
                    continue;
                }
                boolean visible = true;
                for (Pair p : asteroids) {      //check if the current asteroid is blocked by another
                    if (m == p.a && n == p.b) {      //comparing some asteroid
                        continue;
                    }
                    if (x == p.a && y == p.b) {
                        continue;
                    }
                    if ((m - x) / (double) (n - y) == (p.a - x) / (double) (p.b - y) && (new Pair(m, n).dist(new Pair(x, y)) > p.dist(new Pair(x, y)))) {
                        if ((n - y) * (p.b - y) < 0) {      //opposite sides
                            continue;
                        }
                        if ((m - x) * (p.a - x) < 0) {      //opposite sides
                            continue;
                        }
                        visible = false;
                        break;
                    }
                }
                if (visible) {
                    Pair remove = new Pair(m, n);
                    double angle = Math.atan2(remove.a - x, remove.b - y);      
                    if(angle < -Math.PI/2) {        //this math works don't question it
                        angle += 2*Math.PI;
                    }
                    Asteroid ast = new Asteroid(remove, angle);
                    toRemove.add(ast);
                }
            }
        }
        Collections.sort(toRemove);
        System.out.println("y: " + toRemove.get(199).p.a + "  x: " + toRemove.get(199).p.b);
    }
}

class Pair {
    int a, b;
    
    public Pair(int a, int b) {
        this.a = a;
        this.b = b;
    }
    
    public double dist(Pair p) {
        return Math.abs(p.a - a) + Math.abs(p.b - b);
    }
}

class Asteroid implements Comparable<Asteroid> {
    Pair p;
    double angle;
    
    public Asteroid(Pair p, double angle) {
        this.p = p;
        this.angle = angle;
    }
    
    public int compareTo(Asteroid a) {
        if (this.angle < a.angle) {
            return -1;
        } else if (a.angle < this.angle) {
            return 1;
        }
        return 0;
    }
}