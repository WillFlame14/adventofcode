#include <iostream>
#include <sstream>
#include <fstream>
#include <set>
#include <vector>
#include <algorithm>

using namespace std;

int test[4];

bool contains(vector<int> vec, int element);
bool check(int test[], int after[]);
void attempt(int (&test)[4], int, int, int, int);
void reduce(vector<int> &found, vector<int> ops[16]);

bool part1 = false;  //set to false for part 2 to save time

int main() {
    ifstream inFile;    //input file
    string line;        //line currently being read

    vector<int> ops[16];        //opcode possibilities for all 16 numbers
    vector<int> found;          //opcodes that have been definitively assigned to one number

    inFile.open("input.txt");
    if (!inFile) {
        cerr << "Unable to open file input.txt";
        exit(1);   // call system to stop
    }
    getline(inFile, line);

    int totalsuccess = 0, counter = 0;

    while(1) {
        vector<int> possibleops;
        int index = line.find("[");
        int before[] = {line[index + 1]-48, line[index + 4]-48, line[index + 7]-48, line[index + 10]-48};

        getline(inFile, line);
        istringstream ss(line);
        string token;

        getline(ss, token, ' ');
        int d = stoi(token);
        getline(ss, token, ' ');
        int a = stoi(token);
        getline(ss, token, ' ');
        int b = stoi(token);
        getline(ss, token, ' ');
        int c = stoi(token);

        int success = 0;

        for(int i = 0; i < 4; i++) {        //set test to before
            test[i] = before[i];
        }

        getline(inFile, line);
        int after[] = {line[index + 1]-48, line[index + 4]-48, line[index + 7]-48, line[index + 10]-48};

        if(ops[d].size() == 1 && !part1) {        //this opcode has been determined, so grab the next 2 lines and continue
            getline(inFile, line);
            getline(inFile, line);
            if(line.find("Before") != string::npos) {
                continue;
            }
            break;      //there are no more example cases
        }

        for(int attempttype = 0; attempttype < 16; attempttype++) {
            if(contains(found, attempttype) && !part1) {      //if the opcode has already been found, do not need to check again
                continue;
            }
            attempt(test, a, b, c, attempttype);        //modifies the test array according to attempttype
            if (check(test, after)) {          //if test now matches after
                success++;
                possibleops.push_back(attempttype);
            }
            for(int i = 0; i < 4; i++) {        //reset test back to before
                test[i] = before[i];
            }
        }

        if(success == 1) {      //only matches one opcode
            ops[d].clear();
            ops[d].push_back(possibleops.at(0));    //clear the original contents so it only contains this one opcode
            found.push_back(possibleops.at(0));
            reduce(found, ops);         //remove all instances of this opcode in other vectors
        }
        else if(ops[d].size() == 0) {       //vector is currently empty, add all possible opcodes in
            if(possibleops.size() == 0) {
                cout << counter << " AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" << endl;
            }
            for(int i = 0; i < possibleops.size(); i++) {
                ops[d].push_back(possibleops.at(i));
            }
        }
        else {      //vector has some opcodes from before, keep only those that are in both current and before
            vector<int> both;
            for(int i = 0; i < possibleops.size(); i++) {
                if(contains(ops[d], possibleops.at(i))) {       //match cannot be found
                    both.push_back(possibleops.at(i));        //add to new vector
                }
            }
            ops[d].clear();
            for(int i = 0; i < both.size(); i++) {      //add in all the elements
                ops[d].push_back(both.at(i));
            }
            reduce(found, ops);
        }
        possibleops.clear();
        counter++;

        if(success > 2) {
            totalsuccess++;
        }
        success = 0;

        getline(inFile, line);
        getline(inFile, line);

        if(line.find("Before") == string::npos) {
            break;
        }
    }
    inFile.close();
    cout << totalsuccess << endl;     //used for part 1 only

    ifstream inFile2;
    inFile2.open("input2.txt");
    if (!inFile2) {
        cerr << "Unable to open file input2.txt";
        exit(1);   // call system to stop
    }

    int curr[] = {0, 0, 0, 0};

    for(int i = 0; i < 956; i++) {
        string line, token;
        getline(inFile2, line);
        istringstream ss(line);

        getline(ss, token, ' ');
        int d = stoi(token);
        getline(ss, token, ' ');
        int a = stoi(token);
        getline(ss, token, ' ');
        int b = stoi(token);
        getline(ss, token, ' ');
        int c = stoi(token);

        attempt(curr, a, b, c, ops[d].at(0));
    }

    for(int i = 0; i < 4; i++) {
        cout << curr[i] << " ";
    }
}

void reduce(vector<int> &found, vector<int> ops[16]) {
    bool reduced = true;

    while(reduced) {
        reduced = false;
        for(int i = 0; i < 16; i++) {
            //delete all instances that have been found
            for(int j = 0; j < found.size(); j++) {
                if(contains(ops[i], found.at(j)) && ops[i].size() != 1) {
                    ops[i].erase(find(ops[i].begin(), ops[i].end(), found.at(j)));
                }
            }

            //if this vector now has one element only
            if(ops[i].size() == 1 && !contains(found, ops[i].at(0))) {
                found.push_back(ops[i].at(0));
                reduced = true;
            }
        }
    }
}

//check if vec contains element
bool contains(vector<int> vec, int element) {
    return find(vec.begin(), vec.end(), element) != vec.end();
}

//compare elements of test and after
bool check(int test[], int after[]) {
    for(int i = 0; i < 4; i++) {
        if(test[i] != after[i]) {
            return false;
        }
    }
    return true;
}

//perform operation with number attempttype using values a, b, c on array test
void attempt(int (&test)[4], int a, int b, int c, int attempttype) {
    switch (attempttype) {
        case 0:     //addr
            test[c] = test[a] + test[b];
            break;
        case 1:     //addi
            test[c] = test[a] + b;
            break;
        case 2:     //mulr
            test[c] = test[a] * test[b];
            break;
        case 3:     //muli
            test[c] = test[a] * b;
            break;
        case 4:     //banr
            test[c] = test[a] & test[b];
            break;
        case 5:     //bani
            test[c] = test[a] & b;
            break;
        case 6:     //borr
            test[c] = test[a] | test[b];
            break;
        case 7:     //bori
            test[c] = test[a] | b;
            break;
        case 8:     //setr
            test[c] = test[a];
            break;
        case 9:     //seti
            test[c] = a;
            break;
        case 10:    //gtir
            test[c] = (a > test[b]) ? 1 : 0;
            break;
        case 11:    //gtri
            test[c] = (test[a] > b) ? 1 : 0;
            break;
        case 12:    //gtrr
            test[c] = (test[a] > test[b]) ? 1 : 0;
            break;
        case 13:    //eqir
            test[c] = (a == test[b]) ? 1 : 0;
            break;
        case 14:    //eqri
            test[c] = (test[a] == b) ? 1 : 0;
            break;
        case 15:    //eqrr
            test[c] = (test[a] == test[b]) ? 1 : 0;
            break;
    }
}
