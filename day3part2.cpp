#include <iostream>
#include <fstream>
#include <unordered_map>

int main() {
    std::string line;
    std::ifstream input("input.txt");
    std::unordered_map<int, int>grid[20000];
    std::unordered_map<int, int>dist[20000];

    std::string val;
    input >> val;
    int loc_x = 10000, loc_y = 10000;

    grid[10000].insert(std::make_pair(10000, 10000));
    long distance = 0;
    while(val != "Z") {
        int num = stoi(val.substr(1));
        switch(val[0]) {
        case 'U':
            for(int i = 0; i < num; i++) {
                grid[loc_y].insert(std::make_pair(loc_x, loc_x));
                dist[loc_y].insert(std::make_pair(loc_x, distance));
                loc_y++;
                distance++;
            }
            break;
        case 'D':
            for(int i = 0; i < num; i++) {
                grid[loc_y].insert(std::make_pair(loc_x, loc_x));
                dist[loc_y].insert(std::make_pair(loc_x, distance));
                loc_y--;
                distance++;
            }
            break;
        case 'L':
            grid[loc_y].insert(std::make_pair(loc_x - num, loc_x));
            for(int i = 0; i < num; i++) {
                dist[loc_y].insert(std::make_pair(loc_x - i, distance));
                distance++;
            }
            loc_x -= num;
            break;
        case 'R':
            grid[loc_y].insert(std::make_pair(loc_x, loc_x + num));
            for(int i = 0; i < num; i++) {
                dist[loc_y].insert(std::make_pair(loc_x + i, distance));
                distance++;
            }
            loc_x += num;
            break;
        }
        input >> val;
    }

    input >> val;
    loc_x = 10000;
    loc_y = 10000;

    int smallest = 99999;
    distance = 0;

    while(val != "Z") {
        int num = stoi(val.substr(1));
        switch(val[0]) {
        case 'U':
            for(int i = 0; i < num; i++) {
                for(auto it = grid[loc_y].begin(); it != grid[loc_y].end(); it++) {
                    if(loc_x >= it->first && loc_x <= it->second && (distance + dist[loc_y].find(loc_x)->second) < smallest) {
                        if(std::abs(loc_y - 10000) + std::abs(loc_x - 10000) == 0) {
                            continue;
                        }
                        std::cout << smallest << std::endl;
                        smallest = distance + dist[loc_y].find(loc_x)->second;
                    }
                }
                loc_y++;
                distance++;
            }
            break;
        case 'D':
            for(int i = 0; i < num; i++) {
                for(auto it = grid[loc_y].begin(); it != grid[loc_y].end(); it++) {
                    if(loc_x >= it->first && loc_x <= it->second && (distance + dist[loc_y].find(loc_x)->second) < smallest) {
                        if(std::abs(loc_y - 10000) + std::abs(loc_x - 10000) == 0) {
                            continue;
                        }
                        smallest = distance + dist[loc_y].find(loc_x)->second;
                    }
                }
                loc_y--;
                distance++;
            }
            break;
        case 'L':
            for(int i = 0; i < num; i++) {
                for(auto it = grid[loc_y].begin(); it != grid[loc_y].end(); it++) {
                    if(loc_x >= it->first && loc_x <= it->second && (distance + dist[loc_y].find(loc_x)->second) < smallest) {
                        if(std::abs(loc_y - 10000) + std::abs(loc_x - 10000) == 0) {
                            continue;
                        }
                        smallest = distance + dist[loc_y].find(loc_x)->second;
                    }
                }
                loc_x--;
                distance++;
            }
            break;
        case 'R':
            for(int i = 0; i < num; i++) {
                for(auto it = grid[loc_y].begin(); it != grid[loc_y].end(); it++) {
                    if(loc_x >= it->first && loc_x <= it->second && (distance + dist[loc_y].find(loc_x)->second) < smallest) {
                        if(std::abs(loc_y - 10000) + std::abs(loc_x - 10000) == 0) {
                            continue;
                        }
                        smallest = distance + dist[loc_y].find(loc_x)->second;
                    }
                }
                loc_x++;
                distance++;
            }
            break;
        }
        input >> val;
    }

    std::cout << smallest;

    return 0;
}
