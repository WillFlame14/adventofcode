import java.io.*;
import java.util.*;

public class AdventOfCode2018_7_2 {
    
    static HashMap<Integer, ArrayList<Integer>> matrix;
    static HashMap<Integer, ArrayList<Integer>> requirements;
    
    public static void main(String[] args)throws IOException {
        
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        
        matrix = new HashMap<>(26);
        requirements = new HashMap<>(26);
        
        for(int i = 0; i < 101; i++) {
            String[] parts = br.readLine().split(" ");
            int head = (int)parts[1].charAt(0) - 64;
            int tail = (int)parts[7].charAt(0) - 64;
            
            ArrayList<Integer> ss;
            
            if(!matrix.containsKey(head)) {
                ss = new ArrayList<>(26);
                ss.add(tail);
                matrix.put(head, ss);
            }
            else {
                ss = matrix.get(head);
                ss.add(tail);
                matrix.put(head, ss);
            }
            
            if(!requirements.containsKey(tail)) {
                ss = new ArrayList<>(26);
                ss.add(head);
                requirements.put(tail, ss);
            }
            else {
                ss = requirements.get(tail);
                ss.add(head);
                requirements.put(tail, ss);
            }
        }
        
        Set<Integer> notleaf = matrix.keySet();
        Set<Integer> notleaf2 = requirements.keySet();
        
        System.out.println(notleaf);
        System.out.println(notleaf2);
        
        int current = -1;
        boolean[] visited = new boolean[27];
        Arrays.fill(visited, false);
        
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
        
        int[] available = new int[5];
        int[] currentlyWorking = new int[5];
        boolean[] completed = new boolean[27];
        Arrays.fill(completed, false);
        int next = 0;
        long totaltime = 0;
        boolean forcedcontinue = false;
        
        String completion = "";
        
        HashSet<Integer> waitinglist = new HashSet<>(26);
        
        while(true) {
            if(next == -1) {        //all workers are busy
                int min = 100;
                for(int i = 0; i < available.length; i++) {
                    if(available[i] < min && available[i] != 0) {       //prevents workers that have nothing to do
                        min = available[i];
                    }
                }
                
                totaltime += min;       //enough time has passed for the closest elf to finish
                
                for(int i = 0; i < available.length; i++) {
                    if(available[i] != 0) {
                        available[i] -= min;        //substract the time from all elves that are actually working
                        if (available[i] == 0) {
                            completed[currentlyWorking[i]] = true;      //completed
                            completion += (char)(currentlyWorking[i] + 64);
                            System.out.println("completed " + completion);
                        }
                    }
                }
                next = findNext(available);     //get the next available one
                
                if(completion.length() == 26) {
                    break;
                }
                
                if(forcedcontinue) {        //need to find a new current
                    ArrayList<Integer> fromadj = checkCompleted(matrix.get(current).toArray(), visited, completed);
                    for(int i: fromadj) {
                        readypool.add(i);
                        System.out.println("added " + (char)(i + 64) + " from adj");
                    }
                    
                    ArrayList<Integer> fromwait = checkCompleted(waitinglist.toArray(), visited, completed);
                    for(int i: fromwait) {
                        readypool.add(i);
                        waitinglist.remove(i);
                        System.out.println("added " + (char)(i + 64) + " from wait");
                    }

                    if (readypool.isEmpty() && numTrues(completed) == 26) {
                        break;
                    }
                    
                    Pair p = findMin(readypool.toArray());
                    if(p.a == -1) {
                        System.out.println("BLARING OVERHEAD");
                        next = -1;
                        continue;
                    }
                    else {
                        current = p.b;
                    }
                    readypool.remove(p.b);
                    forcedcontinue = false;
                }
            }
            
            for(int i = 0; i < 5; i++) {
                System.out.println(currentlyWorking[i] + " " + available[i]);
            }
            System.out.println("ready: " + readypool.toString());
            System.out.println("waiting: " + waitinglist.toString());
            
            visited[current] = true;
            System.out.println("working on " + (char)(current + 64));
            available[next] = current + 60;
            currentlyWorking[next] = current;
            
            if(!notleaf.contains(current)) {        //this is a leaf, so cannot get any new nodes
                System.out.println("this is a leaf");
                
                if(readypool.isEmpty()) {
                    if(numTrues(completed) == 26) {
                        break;
                    }
                    System.out.println("FORCED CONTINUE V2");
                    forcedcontinue = true;
                    next = -1;
                }
                Pair p = findMin(readypool.toArray());
                if(p.a == -1) {
                    System.out.println("BLARING LEAF");
                    next = -1;
                    continue;
                }
                else {
                    current = p.b;
                }
                readypool.remove(p.b);
                next = findNext(available);
                continue;
            }
            
            System.out.println("possible connections " + matrix.get(current));
            ArrayList<Integer> fromadj = checkCompleted(matrix.get(current).toArray(), visited, completed);
            for (int i : matrix.get(current)) {
                if(fromadj.contains(i)) {
                    readypool.add(i);
                    System.out.println("added " + (char) (i + 64) + " from adj");
                }
                else {
                    waitinglist.add(i);
                    System.out.println("added " + (char) (i + 64) + " from adj to wait");
                }
            }
            
            if(fromadj.isEmpty() && readypool.isEmpty()) {       //no new projects are available. current has not changed.
                System.out.println("FORCED CONTINUE");
                forcedcontinue = true;
                next = -1;
                continue;
            }
            
            int min = 27;
            
            if (readypool.isEmpty() && numTrues(completed) == 26) {
                break;
            }
            
            for(int i: readypool) {     //find the first alphabetically
                if(i < min) {
                    min = i;
                }
            }
            current = min;
            if(min == 27) {
                System.out.println("BLARING NONLEAF");
            }
            readypool.remove(min);
            next = findNext(available);
        }
        System.out.println(totaltime);
    }
    
    public static int findNext(int[] available) {
        for(int i = 0; i < available.length; i++) {
            if(available[i] == 0) {
                return i;
            }
        }
        return -1;  //no one is available
    }
    
    public static int numTrues (boolean[] array) {
        int counter = 0;
        for(boolean b: array) {
            if(b) {
                counter++;
            }
        }
        return counter;
    }
    
    public static Pair findMin (Object[] c) {
        int min = 10000, index = -1;
        for(int i = 0; i < c.length; i++) {
            int cc = Integer.parseInt(c[i] + "");
            if(cc < min) {
                min = cc;
                index = i;
            }
        }
        return new Pair(index, min);
    }
    
    public static ArrayList<Integer> checkCompleted (Object[] c, boolean[] visited, boolean[] completed) {
        ArrayList<Integer> satisfies = new ArrayList<>(26);
        for(int i = 0; i < c.length; i++) {
            int cc = Integer.parseInt(c[i] + "");
            if (!visited[cc]) {
                boolean complete = true;
                for (int j : requirements.get(cc)) {
                    if (!completed[j]) {    //if any of them have not been completed
                        complete = false;
                        break;
                    }
                }
                if (complete) {
                    satisfies.add(cc);
                }
            }
        }
        return satisfies;
    }
}

class Pair {
    int a, b;
    
    Pair(int a, int b) {
        this.a = a;
        this.b = b;
    }
}