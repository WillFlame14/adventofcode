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
            
            if(!map.containsKey(parts[0])) {
                ArrayList<String> list = new ArrayList<>();
                list.add(parts[1]);
                map.put(parts[0], list);
            }
            else {
                map.get(parts[0]).add(parts[1]);
            }
        }
        dfs("YOU", 0);
    }
    
    static HashSet<String> visited = new HashSet<>();
    
    static void dfs(String s, int distance) {
        visited.add(s);
        ArrayList<String> next = map.get(s);
        for (String str : next) {
            if(s.equals("SAN")) {
                System.out.println(distance);
                return;
            }
            if(visited.contains(str) || str.equals("COM")) {      //visited
                continue;
            }
            dfs(str, distance + 1);
        }
    }
}