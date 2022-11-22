#include <iostream>
#include <fstream>

using namespace std;

int main() {
    ifstream myfile;
    myfile.open("input.txt");
    long sum = 0;
    for(int i = 0; i < 100; i++) {
        int mass;
        myfile >> mass;
        int value = mass / 3 - 2;
        while(value > 0) {
            sum += value;
            value = value / 3 - 2;
        }
    }
    std::cout << sum;
    return 0;
}
