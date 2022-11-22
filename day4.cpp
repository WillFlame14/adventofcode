bool find(int digits[], int size);

int main() {
    int counter = 0;

    for(int i = 145852; i <= 616942; i++) {
        int save = i;
        int d0 = save % 10;
        save /= 10;
        int d1 = save % 10;
        save /= 10;
        int d2 = save % 10;
        save /= 10;
        int d3 = save % 10;
        save /= 10;
        int d4 = save % 10;
        save /= 10;
        int d5 = save % 10;
        save /= 10;

        int digits[6];
        digits[0] = d0;
        digits[1] = d1;
        digits[2] = d2;
        digits[3] = d3;
        digits[4] = d4;
        digits[5] = d5;

        if(d0 >= d1 && d1 >= d2 && d2 >= d3 && d3 >= d4 && d4 >= d5) {
            if(find(digits, 6)) {
                counter++;
            }
        }
    }
    std::cout << counter;
    return 0;
}

bool find(int digits[], int size) {
    int counter = 1;
    for(int i = 1; i < size; i++) {
        if(digits[i] == digits[i - 1]) {
            counter++;
        }
        else {
            if(counter == 2) {
                return true;
            }
            counter = 1;
        }
    }
    if(counter == 2) {
        return true;
    }
    return false;
}
