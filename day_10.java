package randomstufftesting;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.io.*;
import javax.swing.*;
import javax.swing.Timer;

class AdventOfCode2018_10 extends JFrame {
    static Colony colony;
    static Timer t;
    static Quad[] stars;
    static Pair fiducial;
    static int length;
    static boolean start;
    static long time;

    public RandomStuffTesting() {

        // 2... Create content pane, set layout
        JPanel content = new JPanel();        // Create a content pane
        content.setLayout(new FlowLayout()); // Use BorderLayout for panel

        DrawArea board = new DrawArea(1810, 970);
        content.add(board); // Output area

        // 4... Set this window's attributes.
        setContentPane(content);
        setResizable(false);
        setTitle("Life Simulation");
        pack();
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setLocationRelativeTo(null);           // Center window.
        
        fiducial = new Pair(0, 0);
        start = false;
        time = 0;
        
        colony = new Colony(stars);
        Movement moveColony = new Movement(colony); //ActionListener
        findclosest(stars);
        t = new Timer(1000, moveColony);     //set up timer
        t.start();  
    }
    
    public static void main(String[] args)throws IOException {       
        
        length = 379;
        
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        stars = new Quad[length];
        
        for(int i = 0; i < length; i++) {
            String[] parts = br.readLine().split("<");
            int aa = Integer.parseInt(parts[1].substring(0, parts[1].indexOf(",")).trim());
            int bb = Integer.parseInt(parts[1].substring(parts[1].indexOf(",") + 1, parts[1].indexOf(">")).trim());
            int cc = Integer.parseInt(parts[2].substring(0, parts[2].indexOf(",")).trim());
            int dd = Integer.parseInt(parts[2].substring(parts[2].indexOf(",") + 1, parts[2].indexOf(">")).trim());
            stars[i] = new Quad(aa, bb, cc, dd);
        }
        RandomStuffTesting window = new RandomStuffTesting();
        window.setVisible(true);
        window.toFront();
        window.requestFocus();
    }
    
    public static double distance (Pair p, Quad q) {
        return Math.sqrt(Math.pow(q.a - p.a, 2) + Math.pow(q.b - p.b, 2));
    }
    
    public static void findclosest(Quad[] stars) {
        double totaldistance = Double.MAX_VALUE;
        while(totaldistance > 23000) {
            colony.advance(start);
            Pair centre = new Pair(0, 0);
            for(Quad q: stars) {
                centre.a += q.a;
                centre.b += q.b;
            }
            centre.a /= stars.length;
            centre.b /= stars.length;
            
            totaldistance = 0;
            
            for(Quad q: stars) {
                totaldistance += distance(centre, q);
            }
            System.out.println(totaldistance);
        }
        start = true;
    }


    class DrawArea extends JPanel {
        public DrawArea(int width, int height) {
            this.setPreferredSize(new Dimension(width, height)); // size
        }

        public void paintComponent(Graphics g) {
            colony.show (g);
        }
    }

    class Movement implements ActionListener{
        private Colony colony;

        public Movement(Colony col) {
            colony = col;
        }

        public void actionPerformed(ActionEvent event) {
            colony.advance(start);
            repaint();
        }
    }
}

class Colony {
    private static Quad[] stars;

    public Colony (Quad[] stars) {
        this.stars = stars;
    }
    
    public void show (Graphics g) {
        g.setColor(Color.black);
        for (Quad q: stars) {
            g.fillRect (q.a*5 - 300, q.b*5 - 100, 5, 5);    //draw life form
        }
    }
    
    public void advance(boolean start) {
        RandomStuffTesting.time++;
        for (Quad q: stars) {
            q.a += q.c;
            q.b += q.d;
        }
        if(start)
            System.out.println(RandomStuffTesting.time);    //just close the screen when the word appears
    }
}

class Pair {
    int a, b;
    
    public Pair(int a, int b) {
        this.a = a;
        this.b = b;
    }
}

class Quad {
    int a, b, c, d;
    
    public Quad(int a, int b, int c, int d) {
        this.a = a;
        this.b = b;
        this.c = c;
        this.d = d;
    }
}