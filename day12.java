package main;

import java.util.*;
import java.io.*;

public class Main {
  
    public static void main(String[] args)throws IOException {
        //BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        
        HashSet<String> saves = new HashSet<>();
        System.out.println(lcm(lcm(84032, 161428), 231614));
        
        MoonSystem system = new MoonSystem();     //pos, vel
        system.addMoon(new Moon(new Triple(8, 0, 8), new Triple(0, 0, 0)));
        system.addMoon(new Moon(new Triple(0, -5, -10), new Triple(0, 0, 0)));
        system.addMoon(new Moon(new Triple(16, 10, -5), new Triple(0, 0, 0)));
        system.addMoon(new Moon(new Triple(19, -10, -7), new Triple(0, 0, 0)));
        
        int counter = 0;
        
        while(true) {       //set this loop to 1000 and then print system.totalEnergy() for part 1
            system.affectGravity();
            system.updatePos();
            String s = system.toString();
            if(saves.contains(s)) {
                break;
            }
            saves.add(s);
            counter++;
        }
        System.out.println(counter);
    }
    
    public static double gcd(double a, double b) {
        if(a < b) {
            double temp = b;
            b = a;
            a = temp;
        }
        if(a == 0) {
            return b;
        }
        if(b == 0) {
            return a;
        }
        return gcd(b, a % b);
    }
    
    public static double lcm(double a, double b) {
        return (a * b) / gcd(a, b);
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

class Triple {
    double x, y, z;
    
    public Triple(double x, double y, double z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }
}

class Moon {
    Triple pos, vel;
    
    public Moon(Triple pos, Triple vel) {
        this.pos = pos;
        this.vel = vel;
    }
    
    public double energy() {
        return (Math.abs(pos.x) + Math.abs(pos.y) + Math.abs(pos.z)) * (Math.abs(vel.x) + Math.abs(vel.y) + Math.abs(vel.z));
    }
}

class MoonSystem {
    ArrayList<Moon> moons;
    
    public MoonSystem() {
        moons = new ArrayList<>();
    }
    
    public void addMoon(Moon moon) {
        moons.add(moon);
    }
    
    public void affectGravity() {
        for(int i = 0; i < moons.size(); i++) {
            Moon current = moons.get(i);
            for(int j = 0; j < moons.size(); j++) {
                if(j == i) {
                    continue;
                }
                Moon second = moons.get(j);
                if(second.pos.x != current.pos.x) {
                    current.vel.x += (second.pos.x > current.pos.x)?1:-1;
                }
                if(second.pos.y != current.pos.y) {
                    current.vel.y += (second.pos.y > current.pos.y)?1:-1;
                }
                if(second.pos.z != current.pos.z) {
                    current.vel.z += (second.pos.z > current.pos.z)?1:-1;
                }
            }
        }
    }
    
    public void updatePos() {
        for(int i = 0; i < moons.size(); i++) {
            Moon current = moons.get(i);
            current.pos.x += current.vel.x;
            current.pos.y += current.vel.y;
            current.pos.z += current.vel.z;
        }
    }
    
    public double totalEnergy() {
        double total = 0;
        for(int i = 0; i < moons.size(); i++) {
            total += moons.get(i).energy();
        }
        return total;
    }
    
    public String toString() {
        String s = "";
        for(int i = 0; i < moons.size(); i++) {
            Moon current = moons.get(i);
            s += current.pos.z + "\t" + current.vel.z + "\t";       //change this to x, y, z and then find lcm of all 3 numbers
        }
        return s;
    }
}