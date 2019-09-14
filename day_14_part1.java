import java.util.*;

public class AdventOfCode2018_14_1 {
    
    public static void main(String[] args) {
        int prefix = 640441;
        ArrayList<Integer> board = new ArrayList<>(prefix + 10);
        
        board.add(3);
        board.add(7);
        int index1 = 0;
        int index2 = 1;
        
        while(board.size() < prefix + 10) {
            int sum = board.get(index1) + board.get(index2);
//            System.out.println(sum);
            
            if(sum >= 10) {
                board.add(1);
                board.add(sum % 10);
            }
            else {
                board.add(sum);
            }
            
            index1 += 1 + board.get(index1);
            index2 += 1 + board.get(index2);
            
            while(index1 >= board.size()) {
                index1 -= board.size();
            }
            while(index2 >= board.size()) {
                index2 -= board.size();
            }
//            System.out.println(board);
        }
        for(int i = 0; i < 10; i++) {
            System.out.print(board.get(prefix + i));
        }
    }
}