import java.io.*;
import java.util.*;

public class AdventOfCode2018_7_1 {
    
    public static void main(String[] args)throws IOException {
        
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        
        HashMap<Integer, ArrayList<Integer>> matrix = new HashMap<>(26);
        HashMap<Integer, ArrayList<Integer>> requirements = new HashMap<>(26);
        
        for(int i = 0; i < 101; i++) {
            String[] parts = br.readLine().split(" ");
            int head = (int)parts[1].charAt(0) - 64;
            int tail = (int)parts[7].charAt(0) - 64;
            
            if(!matrix.containsKey(head)) {
                ArrayList<Integer> ss = new ArrayList<>(26);
                ss.add(tail);
                matrix.put(head, ss);
            }
            else {
                ArrayList<Integer> ss = matrix.get(head);
                ss.add(tail);
                matrix.put(head, ss);
            }
            
            if(!requirements.containsKey(tail)) {
                ArrayList<Integer> ss = new ArrayList<>(26);
                ss.add(head);
                requirements.put(tail, ss);
            }
            else {
                ArrayList<Integer> ss = requirements.get(tail);
                ss.add(head);
                requirements.put(tail, ss);
            }
        }
        
        Set<Integer> notleaf = matrix.keySet();
        Set<Integer> notleaf2 = requirements.keySet();
        
        int current = -1;
        boolean[] visited = new boolean[27];
        
        HashSet<Integer> readypool = new HashSet<>(26);
        
        for(int i = 1; i <= 26; i++) {
            if(!notleaf2.contains(i)) {
                if(current == -1) {
                    current = i;
                }
                else {
                    readypool.add(i);
                }
            }
        }
        
        while(true) {
            visited[current] = true;
            System.out.print((char)(current + 64));
            if(!notleaf.contains(current)) {        //this is a leaf, so cannot get any new nodes
                int min = 27;
                if(readypool.isEmpty()) {
                    break;
                }
                for (int i : readypool) {     //find the first alphabetically
                    if (i < min) {
                        min = i;
                    }
                }
                current = min;
                readypool.remove(min);
                continue;
            }
            for(int i: matrix.get(current)) {   //dump all non-visited nodes
                if(!visited[i]) {
                    boolean complete = true;
                    for(int j: requirements.get(i)) {
                        if(!visited[j]) {   //if any of them have not been visited (completed)
                            complete = false;
                            break;
                        }
                    }
                    if(complete)
                        readypool.add(i);
                }
            }
            
            int min = 27;
            
            if (readypool.isEmpty()) {
                break;
            }
            
            for(int i: readypool) {     //find the first alphabetically
                if(i < min) {
                    min = i;
                }
            }
            current = min;
            readypool.remove(min);
        }
    }
}