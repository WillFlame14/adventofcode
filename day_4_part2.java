import java.io.*;
import java.util.*;

public class AdventOfCode2018_day4_2 {
    public static void main(String[] args)throws IOException {
        
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        
        ArrayList<String> ordered = new ArrayList<>(966);
        HashMap<String, Quad> values = new HashMap<>(966);
        
        for(int i = 0; i < 966; i++) {
            String s = br.readLine();
            String[] parts = s.split(" ");
            
            int month = Integer.parseInt(parts[0].substring(6, 8));
            int date = Integer.parseInt(parts[0].substring(9, 11));
            int hour = Integer.parseInt(parts[1].substring(0, 2));
            int minute = Integer.parseInt(parts[1].substring(3, 5));
            values.put(s, new Quad(month, date, hour, minute));
            
            if(ordered.isEmpty()) {
                ordered.add(s);
                continue;
            }
            
            int x = ordered.size();     //fixed at a specific value
            for(int j = 0; j < x; j++) {
                if(values.get(ordered.get(j)).a > month) {
                    ordered.add(j, s);
                    break;
                }
                else if(values.get(ordered.get(j)).a == month && values.get(ordered.get(j)).b > date) {
                    ordered.add(j, s);
                    break;
                }
                else if(values.get(ordered.get(j)).a == month && values.get(ordered.get(j)).b == date && values.get(ordered.get(j)).c > hour) {
                    ordered.add(j, s);
                    break;
                }
                else if(values.get(ordered.get(j)).a == month && values.get(ordered.get(j)).b == date && values.get(ordered.get(j)).c == hour && values.get(ordered.get(j)).d > minute) {
                    ordered.add(j, s);
                    break;
                }
                if(j == ordered.size() - 1) {       //add to end
                    ordered.add(s);
                }
            }
        }
        
        //all right, now the list is ordered
        
        int guardnum = -1, starthour = -1, startmin = -1;
        HashMap<Integer, Integer> sleep = new HashMap<>(1000);
        HashMap<Integer, ArrayList<Integer>> minutecount = new HashMap<>(1000);
        
        for(int i = 0; i < 966; i++) {
            String s = ordered.get(i);
            String[] parts = s.split(" ");
            
            if(parts[2].equals("Guard")) {
                guardnum = Integer.parseInt(parts[3].substring(1));  //substring to remove the #
            }
            else if(parts[2].equals("falls")) {
                starthour = Integer.parseInt(parts[1].substring(0, 2));
                startmin = Integer.parseInt(parts[1].substring(3, 5));
            }
            else {
                int hour = Integer.parseInt(parts[1].substring(0, 2));
                int min = Integer.parseInt(parts[1].substring(3, 5));
                int mins = (hour - starthour) * 60 + (min - startmin);
                if(!sleep.containsKey(guardnum)) {
                    sleep.put(guardnum, mins);
                    ArrayList<Integer> ss = new ArrayList<>(60);
                    for(int j = 0; j < 60; j++) {
                        ss.add(0);
                    }
                    Collections.fill(ss, 0);
                    if(min < startmin) {        //an hour passed
                        for(int j = startmin; j < 60; j++) {
                            ss.set(j, ss.get(j)+ 1);        //increment at j by 1
                        }
                        for(int j = 0; j < min; j++) {
                            ss.set(j, ss.get(j)+ 1);
                        }
                    }
                    else {
                        for(int j = startmin; j < min; j++) {
                            ss.set(j, ss.get(j)+ 1);
                        }
                    }
                    minutecount.put(guardnum, ss);
                }
                else {
                    int oldtime = sleep.get(guardnum);
                    sleep.replace(guardnum, oldtime + mins);
                    ArrayList<Integer> ss = minutecount.get(guardnum);
                    if(min < startmin) {        //an hour passed
                        for(int j = startmin; j < 60; j++) {
                            ss.set(j, ss.get(j)+ 1);        //increment at j by 1
                        }
                        for(int j = 0; j < min; j++) {
                            ss.set(j, ss.get(j)+ 1);
                        }
                    }
                    else {
                        for(int j = startmin; j < min; j++) {
                            ss.set(j, ss.get(j)+ 1);
                        }
                    }
                    minutecount.put(guardnum, ss);
                }
            }
        }
        
        Object[] minguards = minutecount.keySet().toArray();        
        Object[] minvalues = minutecount.values().toArray();
        
        int max = -1, saveguard = -1, savemin = -1;
        
        for(int i = 0; i < minvalues.length; i++) {
            ArrayList<Object> array =  (ArrayList) minvalues[i];
            for(int j = 0; j < array.size(); j++) {
                int current = Integer.parseInt(array.get(j) + "");
                if(current > max) {
                    max = current;
                    saveguard = Integer.parseInt(minguards[i] + "");
                    savemin = j;
                }
            }
        }
        System.out.println(savemin + " " + saveguard);
    }
}

class Quad {
    int a, b, c, d;
    
    Quad(int a, int b, int c, int d) {
        this.a = a;
        this.b = b;
        this.c = c;
        this.d = d;
    }
}