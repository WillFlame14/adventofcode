#include <iostream>
#include <fstream>

int main() {
    std::string line;
    std::ifstream input("input.txt");

    int orig_array[113];
    int array[113];
    int sizet = 113;

    for(int i = 0; i < sizet; i++) {
        int val;
        input >> val;
        array[i] = val;
        orig_array[i] = val;
    }

    for(int a = 0; a < 99; a++) {
        for(int b = 0; b < 99; b++) {
            orig_array[1] = a;
            orig_array[2] = b;
            for(int l = 0; l < sizet; l++) {
                array[l] = orig_array[l];
            }
            for(int i = 0; i < sizet; i+= 4) {
                if(orig_array[i] == 1) {
                    array[orig_array[i+3]] = array[orig_array[i+1]] + array[orig_array[i+2]];

                }
                else if(orig_array[i] == 2) {
                    array[orig_array[i+3]] = array[orig_array[i+1]] * array[orig_array[i+2]];
                }
                else {
                    break;
                }
            }

            if(array[0] == 19690720) {
                std::cout << a << " " << b << std::endl;
                return 0;
            }
        }
    }

    return 0;
}
