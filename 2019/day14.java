package main;

import java.util.*;
import java.io.*;

public class Main {
    
    static HashMap<String, ArrayList<Quantity>> map;
    static HashMap<String, Long> amount;
    static HashMap<String, Long> leftovers;
  
    public static void main(String[] args)throws IOException {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        map = new HashMap<>();
        amount = new HashMap<>();
        leftovers = new HashMap<>();
        
        for(int i = 0; i < 62; i++) {
            String[] parts = br.readLine().split(" ");
            
            int pointer = 0;
            long quantity;
            String current = parts[pointer], item;
            pointer++;
            ArrayList<Quantity> ingredients = new ArrayList<>(15);
            while(!current.equals("=>")) {
                quantity = Integer.parseInt(current);
                item = parts[pointer];
                pointer++;
                if(item.charAt(item.length() - 1) == ',') {     //cut off dangling comma
                    item = item.substring(0, item.length() - 1);
                }
                ingredients.add(new Quantity(quantity, item));
                current = parts[pointer];
                pointer++;
            }
            quantity = Integer.parseInt(parts[pointer]);
            pointer++;
            item = parts[pointer];
            map.put(item, ingredients);
            amount.put(item, quantity);
        }
        
        long max_fuel = 2910400, ore_req = -1;      //adjust numbers and the increment value until you get close
        while (ore_req < 1000000000000L) {
            max_fuel += 1;
            ore_req = find(new Quantity(max_fuel, "FUEL"));
            //if(max_fuel % 10000 == 0)
                System.out.println(max_fuel + " " + ore_req);
            leftovers.clear();
        }
        System.out.println(max_fuel);
    }
    
    public static long find(Quantity qty) {
        if(leftovers.containsKey(qty.name)) {       //use leftovers first
            if(leftovers.get(qty.name) > qty.num) {
                leftovers.put(qty.name, leftovers.get(qty.name) - qty.num);
                return 0;
            }
            else {
                qty.num -= leftovers.get(qty.name);
                leftovers.remove(qty.name);
            }
        }
        
        if(qty.name.equals("ORE")) {
            return qty.num;
        }
        
        long total = 0, multiplier = (long)Math.ceil((double)qty.num / amount.get(qty.name));
        for(Quantity ingredient: map.get(qty.name)) {
            total += find(new Quantity(ingredient.num * multiplier, ingredient.name));      //recursively find amounts of each ingredient
        }
        if(Math.ceil((double)qty.num / amount.get(qty.name)) != (double)qty.num / amount.get(qty.name)) {       //if the recipe needs to be performed extra times
            if (leftovers.containsKey(qty.name)) {
                leftovers.put(qty.name, (multiplier * amount.get(qty.name) - qty.num) + leftovers.get(qty.name));       //add the extra into leftovers
            } else {
                leftovers.put(qty.name, multiplier * amount.get(qty.name) - qty.num);
            }
        }
        return total;
    }
}

class Quantity {
    long num;
    String name;
    
    public Quantity(long num, String name) {
        this.num = num;
        this.name = name;
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
