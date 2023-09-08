#include <iostream>
#include <fstream>
#include <sstream>
#include <cmath>

#define MAP_WIDTH 32
#define MAP_HEIGHT 32
#define GOBLINS 20
#define ELVES 10
#define ENTITIES (GOBLINS + ELVES)

class Point;
class Node;
class LinkedList;

class Point {
public:
	int x;
	int y;
	Point();
	Point(int x, int y);
	Point* add(int dx, int dy);
	bool equals(Point* p);
	bool earlierThan(Point* p);
	std::string toString();
};

Point::Point() {}

Point::Point(int x, int y) {
	this->x = x;
	this->y = y;
}

Point* Point::add(int dx, int dy) {
	this->x += dx;
	this->y += dy;
	return this;
}

bool Point::equals(Point* p) {
	return this->x == p->x && this->y == p->y;
}

bool Point::earlierThan(Point* p) {
	return this->x < p->x || (this->x == p->x && this->y < p->y);
}

std::string Point::toString() {
	std::ostringstream s;
	s << "(" << this->x << ", " << this->y << ")";

	return s.str();
}

class Entity {
public:
	Point* point;
	int hp;
	bool elf;
	Entity();
	Entity(Point* point, bool elf);
	~Entity();
};

Entity::Entity() {}

Entity::Entity(Point* point, bool elf) {
	this->point = point;
	this->hp = 200;
	this->elf = elf;
}

Entity::~Entity() {
	if (this->point == nullptr) {
		std::cout << "Attempted to deconstruct entity with no point!" << std::endl;
		return;
	}
	delete this->point;
}

class Node {
public:
	Point* p;
	Node* parent;
	Node* next;
	Node* prev;
	Node();
	Node(Point* p, Node* parent);
	~Node();
	Node* clone();
};

Node::Node() {}

Node::Node(Point* p, Node* parent) {
	this->p = p;
	this->parent = parent;
	this->prev = nullptr;
	this->next = nullptr;
}

Node::~Node() {
	delete p;
}

Node* Node::clone() {
	Node* copy = new Node(new Point(this->p->x, this->p->y), this->parent);
	return copy;
}

class LinkedList {
public:
	Node* head;
	Node* tail;
	int length;
	LinkedList();
	~LinkedList();
	void clear();
	void push(Node* n);
	void push_front(Node* n);
	Node* pop();
	bool isEmpty();
	Node* find(Point* p);
};

LinkedList::LinkedList() {
	this->head = nullptr;
	this->tail = nullptr;
	this->length = 0;
}

LinkedList::~LinkedList() {
	this->clear();
}

void LinkedList::clear() {
	Node* curr = this->head;

	while(curr != nullptr) {
		Node* to_delete = curr;
		curr = curr->next;
		delete to_delete;
	}

	this->head = nullptr;
	this->tail = nullptr;
}

void LinkedList::push(Node* n) {
	this->length++;

	if (this->head == nullptr) {
		this->head = n;
		this->tail = n;
		return;
	}

	n->prev = this->tail;
	n->next = nullptr;

	(this->tail)->next = n;
	this->tail = n;
}

void LinkedList::push_front(Node* n) {
	this->length++;

	if (this->head == nullptr) {
		this->head = n;
		this->tail = n;
		return;
	}

	n->prev = nullptr;
	n->next = this->head;

	(this->head)->prev = n;
	this->head = n;
}

Node* LinkedList::pop() {
	if (this->tail == nullptr) {
		std::cout << "Tried to pop from an empty list!" << std::endl;
		exit(0);
	}

	Node* old_tail = this->tail;
	this->tail = old_tail->prev;

	if (this->tail == nullptr) {
		this->head = nullptr;
	}
	else {
		(this->tail)->next = nullptr;
	}

	old_tail->prev = nullptr;
	old_tail->next = nullptr;

	this->length--;

	return old_tail;
}

bool LinkedList::isEmpty() {
	return this->head == nullptr;
}

Node* LinkedList::find(Point* p) {
	Node* curr = this->head;
	while(curr != nullptr) {
		if ((curr->p)->equals(p)) {
			return curr;
		}
		curr = curr->next;
	}
	return nullptr;
}

LinkedList* adjacents(Point* loc);
LinkedList* find_path(char map[MAP_HEIGHT][MAP_WIDTH], Point* start, Point* end);
void print_path(LinkedList* path);
void print_map(char map[MAP_HEIGHT][MAP_WIDTH]);
bool attack_inrange(char map[MAP_HEIGHT][MAP_WIDTH], Point* loc, bool elf);
int main();

LinkedList* adjacents(Point* loc) {
	LinkedList* points = new LinkedList();

	for (int dx = -1; dx <= 1; dx++) {
		for (int dy = -1; dy <= 1; dy++) {
			if (std::abs(dx) + std::abs(dy) != 1) {
				continue;
			}

			Point* new_loc = new Point(loc->x + dx, loc->y + dy);

			if (new_loc->x < 0 || new_loc->x >= MAP_HEIGHT) {
				continue;
			}

			if (new_loc->y < 0 || new_loc->y >= MAP_WIDTH) {
				continue;
			}

			points->push_front(new Node(new_loc, nullptr));
		}
	}

	return points;
}

LinkedList* find_path(char map[MAP_HEIGHT][MAP_WIDTH], Point* start, Point* end) {
	LinkedList frontier{};
	LinkedList next_frontier{};
	LinkedList visited{};

	frontier.push(new Node(new Point(start->x, start->y), nullptr));

	while(!frontier.isEmpty()) {
		Node* curr = frontier.pop();
		visited.push(curr);

		LinkedList* neighbours = adjacents(curr->p);

		while(!neighbours->isEmpty()) {
			Node* neighbour = neighbours->pop();
			Point* p = neighbour->p;

			// Found end, generate path
			if (p->equals(end)) {
				LinkedList* path = new LinkedList();
				path->push_front(new Node(new Point(p->x, p->y), nullptr));

				Node* path_curr = curr;

				while(path_curr != nullptr) {
					path->push_front(path_curr->clone());
					path_curr = path_curr->parent;
				}

				visited.clear();
				frontier.clear();
				next_frontier.clear();
				delete neighbour;
				delete neighbours;

				return path;
			}

			// Already visited or in either frontier
			if (visited.find(p) != nullptr || next_frontier.find(p) != nullptr || frontier.find(p) != nullptr) {
				delete neighbour;
				continue;
			}

			// Valid spot to move to
			if (map[p->x][p->y] == '.') {
				next_frontier.push_front(new Node(new Point(p->x, p->y), curr));
			}
			delete neighbour;
		}

		delete neighbours;

		// When empty, repopulate frontier with next_frontier
		if (frontier.isEmpty() && !next_frontier.isEmpty()) {
			while(!next_frontier.isEmpty()) {
				Node* popped = next_frontier.pop();
				frontier.push_front(popped);
			}
		}
	}
	visited.clear();
	return nullptr;
}

void print_path(LinkedList* path) {
	Node* curr = path->head;
	while(curr != nullptr) {
		Point* p = curr->p;
		std::cout << "(" << p->x << ", " << p->y << ")";

		curr = curr->next;
		if (curr != nullptr) {
			std::cout << " -> ";
		}
	}
	std::cout << std::endl;
}

void print_map(char map[MAP_HEIGHT][MAP_WIDTH]) {
	for (int i = 0; i < MAP_HEIGHT; i++) {
		for (int j = 0; j < MAP_WIDTH; j++) {
			std::cout << map[i][j];
		}
		std::cout << std::endl;
	}
}

void sort_initiatives(Entity** entities) {
	for (int i = 1; i < ENTITIES; i++) {
		Entity* entity = entities[i];
		Point* p = entity->point;
		int curr_index = i;

		for (int j = i - 1; j >= 0; j--) {
			Entity* entity_prev = entities[j];
			Point* p_prev = entity_prev->point;

			// Current is smaller than index j, swap backwards
			if (p->earlierThan(p_prev)) {
				Entity* temp = entities[j];
				entities[j] = entities[curr_index];
				entities[curr_index] = temp;
				curr_index = j;
			}
			else {
				break;
			}
		}
	}
}

bool attack_inrange(char map[MAP_HEIGHT][MAP_WIDTH], Point* loc, bool elf) {
	LinkedList* neighbours = adjacents(loc);

	while(!neighbours->isEmpty()) {
		Node* neighbour = neighbours->pop();
		Point* p = neighbour->p;

		if (map[p->x][p->y] == (elf ? 'G' : 'E')) {
			delete neighbour;
			delete neighbours;
			return true;
		}
		delete neighbour;
	}

	delete neighbours;
	return false;
}

int main() {
	std::fstream input;
	input.open("input.txt");

	char orig_map[MAP_HEIGHT][MAP_WIDTH];
	char map[MAP_HEIGHT][MAP_WIDTH];
	Entity* entities[ENTITIES];
	Entity* entity_map[MAP_HEIGHT][MAP_WIDTH];

	Entity* goblins[GOBLINS];
	Entity* elves[ELVES];

	for (int i = 0; i < ENTITIES; i++) {
		entities[i] = nullptr;
	}

	// Read map
	for (int i = 0; i < MAP_HEIGHT; i++) {
		std::string line;
		input >> line;

		for (int j = 0; j < MAP_WIDTH; j++) {
			orig_map[i][j] = line[j];
		}
	}

	// For part 1, just set ELF_ATK_POWER to 3
	for (int ELF_ATK_POWER = 12; ELF_ATK_POWER < 50; ELF_ATK_POWER++) {
		int count = 0, goblin_count = 0, elf_count = 0;

		for (int i = 0; i < MAP_HEIGHT; i++) {
			for (int j = 0; j < MAP_WIDTH; j++) {
				map[i][j] = orig_map[i][j];
				entity_map[i][j] = nullptr;

				if (map[i][j] == 'G' || map[i][j] == 'E') {
					Entity* entity = new Entity(new Point(i, j), map[i][j] == 'E');
					if (map[i][j] == 'G') {
						goblins[goblin_count] = entity;
						goblin_count++;
					}
					else {
						elves[elf_count] = entity;
						elf_count++;
					}
					entities[count] = entity;
					entity_map[i][j] = entity;
					count++;
				}
			}
		}

		int round = 0;
		bool break_early = false;
		while (goblin_count > 0 && elf_count > 0) {
			sort_initiatives(entities);

			for (int i = 0; i < ENTITIES; i++) {
				Entity* entity = entities[i];

				if (entity->hp <= 0) {
					continue;
				}

				bool elf = entity->elf;
				Entity** opponents = (elf ? goblins : elves);

				if ((elf ? goblin_count : elf_count) == 0) {
					break_early = true;
					break;
				}

				if (!attack_inrange(map, entity->point, elf)) {
					// Not in range, move
					LinkedList* shortest_path = nullptr;
					Point* shortest_path_dest = nullptr;
					int shortest_path_len = 10000;

					// Find nearest opponent target
					for (int j = 0; j < (elf ? GOBLINS : ELVES); j++) {
						Entity* opponent = opponents[j];

						if (opponent->hp <= 0) {
							continue;
						}

						LinkedList* neighbours = adjacents(opponent->point);

						while(!neighbours->isEmpty()) {
							Node* neighbour = neighbours->pop();
							Point* dest = neighbour->p;

							if (map[dest->x][dest->y] != '.') {
								delete neighbour;
								continue;
							}

							LinkedList* path = find_path(map, entity->point, dest);

							if (path == nullptr) {
								delete neighbour;
								continue;
							}

							if (path->length < shortest_path_len || (path->length == shortest_path_len && dest->earlierThan(shortest_path_dest))) {
								if (shortest_path != nullptr) {
									delete shortest_path;
									delete shortest_path_dest;
								}
								shortest_path = path;
								shortest_path_dest = new Point(dest->x, dest->y);
								shortest_path_len = path->length;
							}
							else {
								delete path;
							}
							delete neighbour;
						}

						delete neighbours;
					}

					if (shortest_path != nullptr) {

						Point* curr_loc = entity->point;
						Point* target_loc = ((shortest_path->head)->next)->p;

						map[curr_loc->x][curr_loc->y] = '.';
						map[target_loc->x][target_loc->y] = elf ? 'E' : 'G';

						entity_map[curr_loc->x][curr_loc->y] = nullptr;
						entity_map[target_loc->x][target_loc->y] = entity;

						curr_loc->x = target_loc->x;
						curr_loc->y = target_loc->y;

						delete shortest_path;
						delete shortest_path_dest;
					}
				}

				if (!attack_inrange(map, entity->point, elf)) {
					continue;
				}

				// In range, attack
				LinkedList* neighbours = adjacents(entity->point);

				Entity* best_opponent = nullptr;
				int best_opponent_hp = 1000;

				while(!neighbours->isEmpty()) {
					Node* neighbour = neighbours->pop();
					Point* p = neighbour->p;

					if (map[p->x][p->y] == (elf ? 'G' : 'E')) {
						Entity* opponent = entity_map[p->x][p->y];

						if (opponent->hp > 0 && opponent->hp < best_opponent_hp) {
							best_opponent = opponent;
							best_opponent_hp = opponent->hp;
						}
					}
					delete neighbour;
				}

				delete neighbours;
				best_opponent->hp -= (elf ? ELF_ATK_POWER : 3);

				if (best_opponent->hp <= 0) {
					Point* p = best_opponent->point;
					map[p->x][p->y] = '.';
					entity_map[p->x][p->y] = nullptr;

					(elf ? goblin_count : elf_count)--;
				}
			}

			if (break_early) {
				break;
			}

			round++;
		}

		std::cout << (goblin_count == 0 ? "ELVES" : "GOBLINS") << " WIN @ ATK POWER " << ELF_ATK_POWER << std::endl;

		int entity_hp = 0;

		for (int i = 0; i < GOBLINS + ELVES; i++) {
			if (entities[i]->hp > 0) {
				entity_hp += entities[i]->hp;
			}
		}

		// print_map(map);
		std::cout << "rounds: " << round << " hp: " << entity_hp << " product: " << round * entity_hp << std::endl;

		for (int i = 0; i < GOBLINS + ELVES; i++) {
			delete entities[i];
		}

		if (elf_count == ELVES) {
			std::cout << "No elves died, terminating" << std::endl;
			break;
		}
	}
}
