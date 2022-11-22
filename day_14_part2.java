public class AdventOfCode2018_14_2 {
    
    public static void main(String[] args) {
        String search = "59414";
        StringBuilder board = new StringBuilder();
        
        board.append(3);
        board.append(7);
        int index1 = 0;
        int index2 = 1;
        
        for(int i = 0; i < 100000000; i++) {
            int sum = (board.charAt(index1) - 48) + (board.charAt(index2) - 48);
            
            if(sum >= 10) {
                board.append(1);
                board.append(sum % 10);
            }
            else {
                board.append(sum);
            }
            
            index1 += 1 + (board.charAt(index1) - 48);
            index2 += 1 + (board.charAt(index2) - 48);
            
            while(index1 >= board.length()) {
                index1 -= board.length();
            }
            while(index2 >= board.length()) {
                index2 -= board.length();
            }
        }
        System.out.println(board.indexOf(search));
    }
}