package testing;

import java.util.*;
import java.io.*;

public class Testing {
    
    static HashMap<String, ArrayList<String>> map = new HashMap<>();
    
    public static void main(String[] args)throws IOException {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        
        for(int i = 0; i < 1477; i++) {
            String[] parts = br.readLine().split("\\)");
            
            if(!map.containsKey(parts[1])) {
                ArrayList<String> list = new ArrayList<>();
                list.add(parts[0]);
                map.put(parts[1], list);
            }
            else {
                map.get(parts[1]).add(parts[0]);
            }
        }
        
        Set<String> keys = map.keySet();
        long total = 0;
        
        for(String s: keys) {
            total += find(s);
        }
        System.out.println(total);
    }
    
    static long find(String s) {
        ArrayList<String> strings = map.get(s);
        if(s.equals("COM")) {
            return 0;
        }
        long sum = 0;
        for(String str: strings) {
            sum += find(str) + 1;
        }
        return sum;
    }
}