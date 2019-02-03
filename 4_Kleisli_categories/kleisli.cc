#include <iostream>
#include <math.h>

using namespace std;

template<typename A> class optional {
    bool _is_valid;
    A _value;

    public:
    optional(): _is_valid(false) {}
    optional(A v): _is_valid(true), _value(v) {}
    const bool is_valid() { return _is_valid; }
    const A value() { return _value; }
};

optional<double> safe_root(double x) {
    if(x >= 0) return { sqrt(x) };
    return {};
}

optional<double> safe_reciprocal(double x) {
    if(x != 0) return { 1 / x };
    return {};
}

template<typename T> const auto compose = [](auto m1, auto m2) {
    return [m1, m2](T x) {
        auto p1 = m1(x);

        if(p1.is_valid())
            return m2(p1.value());

        return optional<T> {};
    };
};

template<typename T> optional<T> identity(T x) {
    return { x };
}

const auto safe_root_reciprocal = compose<double>(safe_reciprocal, safe_root);

int main() {
    cout << identity<double>(safe_root_reciprocal(0.25).value()).value() << endl;
    return 0;
}
