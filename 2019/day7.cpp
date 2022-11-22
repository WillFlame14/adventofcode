#include <iostream>
#include <fstream>
#include <vector>

int array[2000];
int orig_array[2000];
int array_a[2000];
int array_b[2000];
int array_c[2000];
int array_d[2000];
int array_e[2000];
std::vector<int> queue_a, queue_b, queue_c, queue_d, queue_e;
int ap = 0, bp = 0, cp = 0, dp = 0, ep = 0;
int sizet = 0;
bool exitall = false, PARTONE = false;
int run(int setting, int input, char letter, bool repeat);

int main() {
    std::string line;
    std::ifstream input("input.txt");

    while(true) {
        int val;
        input >> val;
        array[sizet] = val;
        orig_array[sizet] = val;
        array_a[sizet] = val;
        array_b[sizet] = val;
        array_c[sizet] = val;
        array_d[sizet] = val;
        array_e[sizet] = val;
        sizet++;
        if(val == -314159) {
            break;
        }
    }

    int highest = 0;

    for(int a = 5; a < 10; a++) {
        for(int b = 5; b < 10; b++) {
            if(b == a) {
                continue;
            }
            for(int c = 5; c < 10; c++) {
                if(c == b || c == a) {
                    continue;
                }
                for(int d = 5; d < 10; d++) {
                    if(d == c || d == b || d == a) {
                        continue;
                    }
                    for(int e = 5; e < 10; e++) {
                        if(e == d || e == c || e == b || e == a) {
                            continue;
                        }
                        exitall = false;
                        int result_a, result_b, result_c, result_d, result_e;
                        if(PARTONE) {
                            result_a = run(a - 5, 0, 'a', false);
                            result_b = run(b - 5, result_a, 'b', false);
                            result_c = run(c - 5, result_b, 'c', false);
                            result_d = run(d - 5, result_c, 'd', false);
                            result_e = run(e - 5, result_d, 'e', false);
                        }
                        else {
                            result_a = run(a, 0, 'a', false);
                            result_b = run(b, result_a, 'b', false);
                            result_c = run(c, result_b, 'c', false);
                            result_d = run(d, result_c, 'd', false);
                            result_e = run(e, result_d, 'e', false);
                            while(true) {
                                result_a = run(a, result_e, 'a', true);
                                result_b = run(b, result_a, 'b', true);
                                result_c = run(c, result_b, 'c', true);
                                result_d = run(d, result_c, 'd', true);
                                result_e = run(e, result_d, 'e', true);
                                if(exitall) {
                                    break;
                                }
                            }
                        }
                        if(result_e > highest) {
                            highest = result_e;
                        }

                        //reset all
                        for(int i = 0; i < sizet; i++) {
                            int val = orig_array[i];
                            array_a[i] = val;
                            array_b[i] = val;
                            array_c[i] = val;
                            array_d[i] = val;
                            array_e[i] = val;
                        }
                        ap = 0;
                        bp = 0;
                        cp = 0;
                        dp = 0;
                        ep = 0;
                    }
                }
            }
        }
    }

    std::cout << highest;
    return 0;
}

int run(int setting, int input, char letter, bool repeat) {
    int pointer = 0, save = -1;
    std::vector<int> first;
    int inputs_given = 0;

    //load
    switch(letter) {
        case 'a':
            if(!queue_a.empty()) {
                for (auto i = queue_a.begin(); i != queue_a.end(); ++i) {
                    first.push_back(*i);
                }
                queue_a.clear();
            }
            for(int i = 0; i < sizet; i++) {
                array[i] = array_a[i];
            }
            pointer = ap;
            break;
        case 'b':
            if(!queue_b.empty()) {
                for (auto i = queue_b.begin(); i != queue_b.end(); ++i) {
                    first.push_back(*i);
                }
                queue_b.clear();
            }
            for(int i = 0; i < sizet; i++) {
                array[i] = array_b[i];
            }
            pointer = bp;
            break;
        case 'c':
            if(!queue_c.empty()) {
                for (auto i = queue_c.begin(); i != queue_c.end(); ++i) {
                    first.push_back(*i);
                }
                queue_c.clear();
            }
            for(int i = 0; i < sizet; i++) {
                array[i] = array_c[i];
            }
            pointer = cp;
            break;
        case 'd':
            if(!queue_d.empty()) {
                for (auto i = queue_d.begin(); i != queue_d.end(); ++i) {
                    first.push_back(*i);
                }
                queue_d.clear();
            }
            for(int i = 0; i < sizet; i++) {
                array[i] = array_d[i];
            }
            pointer = dp;
            break;
        case 'e':
            if(!queue_e.empty()) {
                for (auto i = queue_e.begin(); i != queue_e.end(); ++i) {
                    first.push_back(*i);
                }
                queue_e.clear();
            }
            for(int i = 0; i < sizet; i++) {
                array[i] = array_e[i];
            }
            pointer = ep;
            break;
        }

    while(true) {
        int mode = array[pointer];
        pointer++;
        int opcode = mode % 100, mode_a, mode_b, mode_c;
        mode /= 100;
        mode_a = mode % 10;
        mode /= 10;
        mode_b = mode % 10;
        mode /= 10;
        mode_c = mode % 10;

        int a, b, c;
        a = array[pointer];
        pointer++;
        if(opcode == 1 || opcode == 2 || opcode == 7 || opcode == 8) {  //takes 3 parameters
            b = array[pointer];
            pointer++;
            c = array[pointer];
            pointer++;

            if (mode_a == 0) {     //c cannot be immediate mode because problem statement is bad
                a = array[a];
            }
            if (mode_b == 0) {
                b = array[b];
            }

            if(opcode == 1) {
                array[c] = a + b;
            }
            else if(opcode == 2) {
                array[c] = a * b;
            }
            else if(opcode == 7) {
                array[c] = (a < b)?1:0;
            }
            else if(opcode == 8) {
                array[c] = (a == b)?1:0;
            }
        }
        else if(opcode == 3) {
            if(inputs_given == 0) {
                if(!first.empty()) {
                    array[a] = first.back();       //don't increment inputs_given
                    first.pop_back();
                }
                else {
                    if(!repeat) {       //first time around, use phase setting
                        array[a] = setting;
                        inputs_given++;
                    }
                    else {              //otherwise, only use input
                        array[a] = input;
                        inputs_given+= 2;
                    }

                }
            }
            else if(inputs_given == 1) {
                array[a] = input;
                inputs_given++;
            }
            else {
                //waiting for input, go save
                pointer -= 2;      //when loading, ask for this opcode again
                break;
            }
        }
        else if(opcode == 4) {
            if(save != -1) {        //input already ready to send out, move on
                pointer -= 2;       //when loading, ask for this opcode again
                break;
            }
            save = array[a];
        }
        else if(opcode == 5 || opcode == 6) {
            b = array[pointer];
            pointer++;

            if (mode_a == 0) {      //both can be in position mode
                a = array[a];
            }
            if (mode_b == 0) {
                b = array[b];
            }
            if((a != 0 && opcode == 5) || (a == 0 && opcode == 6)) {
                pointer = b;
            }
        }
        else if(opcode == 99) {
            std::cout << "STOP " << std::endl;
            exitall = true;
            break;
        }
        else {
            std::cout << "Unknown opcode " << opcode << " encountered." << std::endl;
            exitall = true;
            break;
        }
    }

    //save

    if(!first.empty()) {        //didn't get to use all inputs
        std::cout << "OH NO WHY" << std::endl;
    }

    switch(letter) {
        case 'a':
            if(inputs_given == 1) {
                queue_a.push_back(input);
                if(inputs_given == 0) {
                    queue_a.push_back(setting);
                }
            }
            for(int i = 0; i < sizet; i++) {
                array_a[i] = array[i];
            }
            ap = pointer;
            break;
        case 'b':
            if(inputs_given == 1) {
                queue_b.push_back(input);
                if(inputs_given == 0) {
                    queue_b.push_back(setting);
                }
            }
            for(int i = 0; i < sizet; i++) {
                array_b[i] = array[i];
            }
            bp = pointer;
            break;
        case 'c':
            if(inputs_given == 1) {
                queue_c.push_back(input);
                if(inputs_given == 0) {
                    queue_c.push_back(setting);
                }
            }
            for(int i = 0; i < sizet; i++) {
                array_c[i] = array[i];
            }
            cp = pointer;
            break;
        case 'd':
            if(inputs_given == 1) {
                queue_d.push_back(input);
                if(inputs_given == 0) {
                    queue_d.push_back(setting);
                }
            }
            for(int i = 0; i < sizet; i++) {
                array_d[i] = array[i];
            }
            dp = pointer;
            break;
        case 'e':
            if(inputs_given == 1) {
                queue_e.push_back(input);
                if(inputs_given == 0) {
                    queue_e.push_back(setting);
                }
            }
            for(int i = 0; i < sizet; i++) {
                array_e[i] = array[i];
            }
            ep = pointer;
            break;
        }
    return save;
}
