#include <iostream>
#include <fstream>

int main() {
    std::string line;
    std::ifstream input("input.txt");

    int sizet = 0;
    int array[2000];

    while(true) {
        int val;
        input >> val;
        if(val == -314159) {
            break;
        }
        array[sizet] = val;
        sizet++;
    }

    int pointer = 0;

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
            array[a] = 5;
        }
        else if(opcode == 4) {
            std::cout << array[a] << std::endl;
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
            std::cout << "STOP" << std::endl;
            break;
        }
        else {
            std::cout << "Unknown opcode " << opcode << " encountered.";
            break;
        }
    }
    return 0;
}
