template<typename Left, typename Right>
class Either {

    enum {
        isLeft,
        isRight
    } tag;

    union {
        Left left;
        Right right;
    };

    public:

    Either(Left val): tag(isLeft), left(val) {}
    Either(Right val): tag(isRight), right(val) {}

    const auto value() { return tag == isLeft ? left : right; }
};

int main() {
    auto e = Either<int, double>(42);
    return e.value();
}
