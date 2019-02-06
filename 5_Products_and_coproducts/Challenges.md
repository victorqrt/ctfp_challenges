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
We would need to provide an accessor for `tag` (see either.cc) :
```c++
int m(Either<int, bool> const & e) {
    if(e.tag == isLeft)
        return e.value();
    return e.value() ? 0 : 1;
}
```