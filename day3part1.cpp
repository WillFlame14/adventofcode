#include <iostream>
#include <fstream>
#include <unordered_map>

int main() {
    std::string line;
    std::ifstream input("input.txt");
    std::unordered_map<int, int>grid[40000];

    std::string val;
    input >> val;
    int loc_x = 20000, loc_y = 20000;
    grid[20000].insert(std::make_pair(20000, 20000));

    while(val != "Z") {
        int num = stoi(val.substr(1));
        switch(val[0]) {
        case 'U':
            for(int i = 0; i <= num; i++) {
                grid[loc_y].insert(std::make_pair(loc_x, loc_x));
                loc_y++;
            }
            break;
        case 'D':
            for(int i = 0; i <= num; i++) {
                grid[loc_y].insert(std::make_pair(loc_x, loc_x));
                loc_y--;
            }
            break;
        case 'L':
            grid[loc_y].insert(std::make_pair(loc_x - num, loc_x));
            loc_x -= num;
            break;
        case 'R':
            grid[loc_y].insert(std::make_pair(loc_x, loc_x + num));
            loc_x += num;
            break;
        }
        input >> val;
    }

    input >> val;
    loc_x = 20000;
    loc_y = 20000;

    int smallest = 99999;

    while(val != "Z") {
        int num = stoi(val.substr(1));
        switch(val[0]) {
        case 'U':
            for(int i = 0; i < num; i++) {
                for(auto it = grid[loc_y].begin(); it != grid[loc_y].end(); it++) {
                    if(loc_x >= it->first && loc_x <= it->second && ((std::abs(loc_y - 20000) + std::abs(loc_x - 20000) < smallest))) {
                        if(std::abs(loc_y - 20000) + std::abs(loc_x - 20000) == 0) {
                            continue;
                        }
                        std::cout << smallest << std::endl;
                        smallest = std::abs(loc_y - 20000) + std::abs(loc_x - 20000);
                    }
                }
                loc_y++;
            }
            break;
        case 'D':
            for(int i = 0; i < num; i++) {
                for(auto it = grid[loc_y].begin(); it != grid[loc_y].end(); it++) {
                    if(loc_x >= it->first && loc_x <= it->second && ((std::abs(loc_y - 20000) + std::abs(loc_x - 20000) < smallest))) {
                        if(std::abs(loc_y - 20000) + std::abs(loc_x - 20000) == 0) {
                            continue;
                        }
                        smallest = std::abs(loc_y - 20000) + std::abs(loc_x - 20000);
                    }
                }
                loc_y--;
            }
            break;
        case 'L':
            for(int i = 0; i < num; i++) {
                for(auto it = grid[loc_y].begin(); it != grid[loc_y].end(); it++) {
                    if(loc_x >= it->first && loc_x <= it->second && ((std::abs(loc_y - 20000) + std::abs(loc_x - 20000) < smallest))) {
                        if(std::abs(loc_y - 20000) + std::abs(loc_x - 20000) == 0) {
                            continue;
                        }
                        smallest = std::abs(loc_y - 20000) + std::abs(loc_x - 20000);
                    }
                }
                loc_x--;
            }
            break;
        case 'R':
            for(int i = 0; i < num; i++) {
                for(auto it = grid[loc_y].begin(); it != grid[loc_y].end(); it++) {
                    if(loc_x >= it->first && loc_x <= it->second && ((std::abs(loc_y - 20000) + std::abs(loc_x - 20000) < smallest))) {
                        if(std::abs(loc_y - 20000) + std::abs(loc_x - 20000) == 0) {
                            continue;
                        }
                        smallest = std::abs(loc_y - 20000) + std::abs(loc_x - 20000);
                    }
                }
                loc_x++;
            }
            break;
        }
        input >> val;
    }

    std::cout << smallest;

    return 0;
}
