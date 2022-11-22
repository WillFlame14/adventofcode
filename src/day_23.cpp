#include  <bits/stdc++.h>

// This solution is a port of my part 1 solution in JavaScript... it's still very slow on part 2.
// Honestly, I blame std::find.

int main() {
	// std::vector<int> cups = { 3, 8, 9, 1, 2, 5, 4, 6, 7 };
	std::vector<int> cups = { 4, 6, 9, 2, 1, 7, 5, 3, 8 };

	int max = 1000000;
	int current = cups[0];

	for(int i = 10; i <= 1000000; i++) {
		cups.push_back(i);
	}

	for(int i = 0; i < 10000000; i++) {
		
		if(i % 100 == 0) {
			std::cout << i << std::endl;
		}

		int destination = (current - 1) % max;

		if(destination <= 0) {
			destination += max;
		}

		int pickup[3];
		int indexes[3];
		int curr_index = std::distance(cups.begin(), std::find(cups.begin(), cups.end(), current));

		for(int j = 0; j < 3; j++) {
			int index = (curr_index + j + 1) % max;
			pickup[j] = cups[index];
			indexes[j] = index;
		}

		std::sort(indexes, indexes + 3, std::greater<int>());

		for(int j = 0; j < 3; j++) {
			cups.erase(cups.begin() + indexes[j]);
		}

		while(std::count(pickup, pickup + 3, destination) == 1) {
			destination--;

			// Wrap around
			if(destination == 0) {
				destination = max;
			}
		}

		cups.insert(cups.begin() + std::distance(cups.begin(), std::find(cups.begin(), cups.end(), destination)) + 1, pickup, pickup + 3);

		int next_index = std::distance(cups.begin(), std::find(cups.begin(), cups.end(), current)) + 1;
		if(next_index == max) {
			next_index = 0;
		}

		current = cups[next_index];
	}

	int one_index = std::distance(cups.begin(), std::find(cups.begin(), cups.end(), 1));

	// std::cout << std::vector<int>(cups.begin() + one_index, v1.begin() + one_index + 2) << std::endl;
	for(const auto i: cups) {
		std::cout << i << ' ';
	}
}
