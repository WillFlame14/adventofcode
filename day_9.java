import java.util.*;

public class AdventOfCode2018_9 {
    
    static ArrayList<Triple> board = new ArrayList<>(7101000);
    
    public static void main(String[] args) {
        int players = 468, marbles = 7101000, currentmarbleindex = 0;
        long[] scores = new long[players];
        long highscore = 0;
        
        board.add(new Triple(0, 0, 0));
        
        for(int marblesplayed = 1; marblesplayed < marbles + 1; marblesplayed++) {
            if(marblesplayed % 23 == 0) {
                int playernum = marblesplayed % players;        //player 0 is really player 468
                
                for (int i = 0; i < 7; i++) {
                    currentmarbleindex = board.get(currentmarbleindex).before;
                }
                Triple remove = board.get(currentmarbleindex);
                scores[playernum] += marblesplayed + remove.value;

                board.get(remove.before).after = remove.after;
                board.get(remove.after).before = remove.before;
                
                currentmarbleindex = remove.after;
                remove.value = -1;
                remove.before = -1;
                remove.after = -1;
                
                if(scores[playernum] > highscore) {
                    highscore = scores[playernum];
                }
            }
            else {
                Triple bef = board.get(board.get(currentmarbleindex).after);
                Triple aft = board.get(bef.after);
                board.add(new Triple(marblesplayed, board.get(currentmarbleindex).after, bef.after));
                bef.after = marblesplayed - (marblesplayed / 23);
                aft.before = marblesplayed - (marblesplayed / 23);
                
                currentmarbleindex = marblesplayed - (marblesplayed / 23);
            }
        }
        System.out.println(highscore);
    }
}

class Triple {
    int value, before, after;
    
    public Triple(int value, int before, int after) {
        this.value = value;
        this.before = before;
        this.after = after;
    }
}