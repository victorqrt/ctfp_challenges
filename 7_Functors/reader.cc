#include <functional>
#include <iostream>
#include <string>

using namespace std;

// Reader functor
template<typename A, typename B, typename R>
function<B(R)> fmap(function<B(A)> f, function<A(R)> g) {
    return [f, g](R r) {
        return f(g(r));
    };
}

// Here, as an example, a function that takes a "hue" an returns string
typedef enum Hue {
    Light,
    Dark
} Hue;

string str_hue(Hue h) {
    return h == Light ? "Its light" : "It's dark";
}

// We use a reader from Color to string
typedef enum Color {
    Black,
    White,
    LightBlue
} Color;

Hue ctoh(Color c) {
    return c == Black ? Dark : Light;
}

// And then lift str_hue
const auto str_color = fmap<Hue, string, Color>(str_hue, ctoh);

int main() {

    cout << str_color(White) << endl;
    return 0;
}