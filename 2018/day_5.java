import java.util.*;
import java.io.*;

public class AdventOfCode2018_day5 {
    
    public static void main(String[] args)throws IOException {       
        
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        
        String save = br.readLine(), s;
        
        boolean complete;
        
        int index = -1, symmetry = -1;
        
        for(int a = 0; a < 26; a++) {
            s = save.replace((char)((int)'a' + a) + "", "");
            s = s.replace((char)((int)'A' + a) + "", "");
            System.out.print((char)((int)'A' + a) + " ");
            complete = false;
            while(!complete) {
                complete = true;
                for(int i = 0; i < s.length(); i++) {
                    if(i != s.length() - 1 && s.substring(i, i+1).toUpperCase().equals(s.substring(i+1, i+2).toUpperCase())) {
                        if(isUppercase(s.substring(i, i+1)) && isUppercase(s.substring(i+1, i+2))) {
                            continue;
                        }
                        else if(!isUppercase(s.substring(i, i+1)) && !isUppercase(s.substring(i+1, i+2))) {
                            continue;
                        }
                        symmetry = 0;
                        while((i - symmetry) >= 0 && (i + symmetry + 2) < s.length() && s.substring(i - symmetry, i - symmetry + 1).toUpperCase().equals(s.substring(i + 1 + symmetry, i + symmetry + 2).toUpperCase())) {
                            if (isUppercase(s.substring(i - symmetry, i - symmetry + 1)) && isUppercase(s.substring(i + 1 + symmetry, i + symmetry + 2))) {
                                break;
                            } else if (!isUppercase(s.substring(i - symmetry, i - symmetry + 1)) && !isUppercase(s.substring(i + 1 + symmetry, i + symmetry + 2))) {
                                break;
                            }
                            symmetry++;
                        }
                        if(symmetry > 0) {
                            symmetry--;
                        }
                        index = i;
//                        System.out.println(index + " " + symmetry);
                        complete = false;
                        break;
                    }
                }
                if(complete) {
                    break;
                }
                s = s.substring(0, index - symmetry) + s.substring(index + 2 + symmetry);
//                System.out.println(s);
            }
            System.out.println(s.length());
        }
    }
    
    public static boolean isUppercase(String s) {
        return s.toUpperCase().equals(s);
    }
}