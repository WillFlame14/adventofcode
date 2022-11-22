public class AdventOfCode2018_11 {
    
    public static void main(String[] args) {
        long[][] board = new long[301][301];
        
        long high = Long.MIN_VALUE;
        long[] answer = {-1, -1, 1};
        
        long sum = 0;
        
        for(int i = 1; i < 301; i++) {
            for(int j = 1; j < 301; j++) {
                long rackid = j + 10, value = (rackid * i + 6878) * rackid;
                String s = "" + value;
                int result = Integer.parseInt("" + s.charAt(s.length() - 3)) - 5;
                sum += result;
                board[i][j] = result;
                
                if(result > high) {
                    answer[0] = j;
                    answer[1] = i;
                    high = result;
                }
            }
        }
        if(sum > high) {
            answer[0] = 1;
            answer[1] = 1;
            answer[2] = 300;
            high = sum;
        }
        
        for(int size = 2; size < 300; size++) {
            long leftvalue = 0;
            for(int i = 1; i < 302 - size; i++) {
                long currentvalue = 0;
                for(int j = 1; j < 302 - size; j++) {
                    if(i == 1 && j == 1) {
                        long power = 0;
                        for (int a = 0; a < size; a++) {
                            for (int b = 0; b < size; b++) {
                                power += board[i + a][j + b];
                            }
                        }
                        leftvalue = power;
                        currentvalue = power;
                    }
                    else if (j == 1) {
                        for(int b = 0; b < size; b++) {
                            leftvalue += - board[i - 1][j + b] + board[i + size - 1][j + b];
                            currentvalue = leftvalue;
                        }
                    }
                    else {
                        for(int a = 0; a < size; a++) {
                            currentvalue += - board[i + a][j - 1] + board[i + a][j + size - 1];
                        }
                    }
                    if(currentvalue > high) {
                        high = currentvalue;
                        answer[0] = j;
                        answer[1] = i;
                        answer[2] = size;
                    }
                }
            }
        }
        System.out.println(answer[0] + " " + answer[1] + " " + answer[2]);
    }
}