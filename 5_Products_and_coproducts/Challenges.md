# 5.8 Challenges

## 1.
Let t1 and t2 be two terminal objects. There is a unique morphism `f: t1 -> t2`, and a unique morphism `g: t2 -> t1`.

f . g is a morphism from t1 to t1. As t1 is terminal, and t1 needs an identity morphism, f . g is identity.
In the same way, g . f is the identity of t2.

`f . g = g . f = id` => t1 and t2 are isomorphic.

As t1 and t2 are terminal, f and g are unique. Therefore, a terminal object in a category is unique up to unique isomorphism.

## 2.
Let a and b be two objects in a poset. A product of a and b is an object c that is both in relation with a and b (we have projection morphisms
`p: c -> a` and `q: c -> b`) such as for any other object c' in relation with both a and b, c' is in relation with c (we have `m: c' -> c`).

The product of a and b would be the "closest" object in relation with both. If the order relation is =<, this is the greatest object
smaller than both a and b. In the case of a total order, this is guaranteed to be their minimum (as a and b are guaranteed to be comparable, there
exists a morphism from one of them to the other).

[//]: # (Embedding => non-surjective)
[//]: # (Collapsing => non-injective)

## 3.
Similarly, and by "reversing the arrows", a coproduct of a and b in a poset is the smallest object greater than a and b. In a total order, this
least upper bound is their maximum.

## 4.
See `either.cc`.

## 5.
We would need to provide an accessor for `tag` (see  `either.cc`) :
```c++
int m(Either<int, bool> const & e) {
    if(e.tag == isLeft)
        return e.value();
    return e.value() ? 0 : 1;
}
```

## 6.
In order for `int` equipped with the `i` and `j` morphisms to be a "better" coproduct for `int, bool` than `Either`, there must exist a unique morphism `m` creating a `Either` from an `int`.

The problem is that `i` and `j` are losing information about the original objects we are going from: as a `bool` is mapped to either 0 or 1 by `j`, we cannot then factorize this morphism "back" to a boolean (or any right or left side of `Either`).

## 7.
The presented morphism `i` also loses information due to a technical difficulty: it does not factorize `Either` for some values of n. Namely, it does not work with `INT_MAX` from `<limits.h>` (or `INT_MAX - 1`).

But what would happen if we assumed the `int` type had no limits and perfectly represented ℤ ?

Let `f` be a morphism from `Either<int, bool>` to `int` and `g` from `int` to `Either<int, bool>`.
Let's define them as:

```c++
int f(Either<int, bool> e) {
    if(e.tag == isRight)
        return e.value() ? 0 : 1;

    int r = e.value(); // e.tag is Left
    return r < 0 ? r : r + 2;
}

Either<int, bool> g(int i) {
    if(i < 0)
        return Either<int, bool>(i);

    if(i > 1)
        return Either<int, bool>(i - 2);

    return Either<int, bool>(i == 0 ? true : false);
}
```

Assuming `int` is actually ℤ, we have `f . g = g . f = id`. That would mean `Either<int, bool>`  and `int` are isomorphic, and as a coproduct is unique up to unique isomorphism, they are "the same" coproduct of `int` and `bool`.

## 8.
We can apply the example of a product with "too much information" found page 64 to coproducts and have something in the likes of:

```c++
// Pseudo code

// No templates, we are in the example of int and bool
class EitherWithoutAmbiguity {

    enum {
        isInt,
        isBool
    } tag;

    typedef struct AbsolutelyCertainBoolean {
        bool isTrue, isFalse;
    } AbsolutelyCertainBoolean;

    union {
        int left;
        AbsolutelyCertainBoolean right;
    };

    public:

    Either(int val): tag(isInt), left(val) {}
    Either(bool val): tag(isBool), right((AbsolutelyCertainBoolean) {val, !val}) {}

    const auto value() { return tag == isInt ? left : right}
};
```

This way we can have two different morphisms from this candidate to `Either`:

```c++
// Pseudo code, would also need tag to be accessible

Either<int, bool> m1(EitherWithoutAmbiguity e) {
    if(e.tag == isInt)
        return Either<int, bool>(e.value());

    return Either<int, bool>(e.value().isTrue);
}

Either<int, bool> m2(EitherWithoutAmbiguity e) {
    if(e.tag == isInt)
        return Either<int, bool>(e.value());

    return Either<int, bool>(!e.value().isFalse);
}
```

Which makes `Either` a superior candidate.