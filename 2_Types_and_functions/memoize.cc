#include <iostream>
#include <map>
#include <chrono>
#include <thread>
#include <stdlib.h>
#include <stdio.h>

using namespace std;

template<typename r_type, typename... arg_types>
auto memoize = []( r_type(*f)(arg_types...) ) {

    map<tuple<arg_types...>, r_type> memtable;

    return [f, memtable](arg_types... args) mutable -> r_type {

        auto arg = make_tuple(args...);
        auto search = memtable.find(arg);

        if(search == memtable.end()) {
            auto result = f(args...);
            memtable[arg] = result;
            return result;
        }

        return search->second;
    };
};

const auto compute = [](auto x) {
    this_thread::sleep_for(chrono::milliseconds(1000));
    this_thread::sleep_for(chrono::milliseconds(9));
    return x;
};

int randint(int n) {
    return rand() % n;
}

int randint_seed(int n, int seed) {
    srand(seed);
    return rand() % n;
}

int fact(int n) {
    int result = 1;
    for(int i=2; i<=n; i++)
        result *= i;
    return result;
}

int main() {

    // 1
    auto mem_compute = memoize<int, int>(compute);
    cout << mem_compute(0) << endl; // Will wait
    cout << mem_compute(0) << endl; // Will not

    // 2
    srand(time(NULL));
    auto mem_rand = memoize<int, int>(randint); // Useless
    cout << mem_rand(999) << endl;
    cout << mem_rand(999) << endl;


    // 3
    auto mem_rand_seed = memoize<int, int, int>(randint_seed); // Somehow more useful
    cout << mem_rand_seed(999, 1) << endl;
    cout << mem_rand_seed(999, 1) << endl;

    // 4
    // a
    auto mem_fact = memoize<int, int>(fact); // Pure function
    cout << mem_fact(5) << endl;

    // b
    auto mem_getchar = memoize<int>(getchar); // Not pure
    cout << (char) mem_getchar() << endl;
    cout << (char) mem_getchar() << endl;

    // c is not pure, a memoized version would only print on the first call
    // d neither, multiple calls with the same parameter would yield different results

    // 5
    // There are 4 functions from Bool to Bool: "id", "not", "true" and "false"

    return 0;
}
