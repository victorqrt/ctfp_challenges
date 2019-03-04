template<typename T>
T identity(T x) {
    return x;
}

template<typename T> const auto compose = [](auto f, auto g) {
    return [f, g](T x) {
        return g(f(x));
    };
};

int square(int x) {
    return x * x;
}

int main() {
    auto f = compose<int>(identity<int>, square);
    return f(3);
}
