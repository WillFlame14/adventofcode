#include  <bits/stdc++.h>

int main() {
	int input[] = {2,1,10,11,0,6};

	std::unordered_map<int, int> memory;
	int dist = 0, num = 0;

	for(int i = 0; i < 30000000; i++) {
		num = i < 6 ? input[i] : dist;

		if(memory.count(num) != 0) {
			dist = i - memory[num];
		}
		else {
			dist = 0;
		}
		memory[num] = i;
	}
	std::cout << num << std::endl;
}
