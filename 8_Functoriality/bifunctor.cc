#include <functional>
#include <iostream>
#include <string>

using namespace std;

/*
    For a better version of this, using a bifunctor generic class to inherit from, we would
    need to get around the fact that virtual templated functions make no sense at runtime.

 */

template<typename A, typename B, typename C, typename D>
function<pair<C, D>(pair<A, B>)> bimap(function<C(A)> f, function<D(B)> g) {

    return (function<pair<C, D>(pair<A, B>)>) [f, g](pair<A, B> p) {
        return make_pair(
            f(p.first),
            g(p.second)
        );
    };
}

// Example

template<typename T>
const auto square = [](T number) { return number * number; };

int main() {

    const auto int_sq = square<int>;
    const auto double_sq = square<double>;

    const auto pair_sq = bimap<int, double, int, double>(int_sq, double_sq);

    const auto my_pair = pair_sq(
        make_pair(7, 2.5)
    );

    cout << my_pair.first << " and " << my_pair.second << endl;

    return 0;
}