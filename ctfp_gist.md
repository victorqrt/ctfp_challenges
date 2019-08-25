# Category theory for programmers - challenges

Those are my answers to the challenges from the Category theory for programmers book, written by Bartosz Milewski, which can be obtained [here](https://github.com/hmemcpy/milewski-ctfp-pdf/).

This gist, the individual sources and markdowns can be found [here](https://github.com/victorqrt/ctfp_challenges).

# 1 Category: the essence of composition

## 1, 2, 3.
```c++
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
```

## 4.
If we consider objects as web pages and morphisms as `<a>` links, then the www is not a category,
as there exists a pair of morphisms such as their composition does not exist: a page may link you
to another web page, but not contain all the links the latter contains.

## 5.
No, because of the same problem: if we consider friendships as morphisms, being friend with
someone does not imply being friend with its friends as well; no composablity.

## 6.
A directed graph is a category if and only if for each path between 3 nodes a->b->c there is
another edge going from a to c.

# 2 Types and functions

```c++
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
```

# 3 Categories great and small

## 1.
### a)
The node is the only object, we only need to add one edge going from itself to itself to get its identity morphism.

### b)
We need to add infinitely many arrows from the node to itself to represent the compositions of the directed edge we started with.

### c)
Each node needs an edge from itself to itself for its identity morphism.

### d)
Assuming none of the marked arrows represent identity, we need to add it. Then we need to add every combination within the a-z alphabet, of every possible length.
This gives us infinetely many arrows from the node to itself.

## 2.
### a)
We have antisymmetry: `A ∈ B and B ∈ A => B = A` so this is a partial order. No total order as there may not be a relation between any two sets.

### b)
Let R be said relation, R is a preorder as it is reflexive and transitive. No preorder though, as if t1 and t2 are pointers to T1 and T2, respectively, t1 R t2 and t2 R t1 does not imply t1 = t2 (casting a pointer to a base class is implicit).

## 3.
### &&
`∀ (a,b,c) ∈ Bool³, (a && b) && c = a && (b && c)`, && is associative.

`∀ a ∈ Bool, a && True = True && a = a`, True is neutral for &&.
Therefore, the Bool set with && is a monoid as set.

### ||
`∀ (a,b,c) ∈ Bool³, (a || b) || c = a || (b || c)`, || is associative.

`∀ a ∈ Bool, a || False = False || a = a`, False is neutral for ||.
Therefore, the Bool set with || is a monoid as set.

## 4.
AND True is identity. AND False is "never".

## 5.
We have three morphisms:
- (+ 0 mod 3) is 'zero'
- (+ 1 mod 3) is 'one'
- (+ 2 mod 3) is 'two'

And the composition rules:

'zero' is identity.

'one' . 'one' is 'two'

'one' . 'two' is identity

'two' . 'two' is 'one'.

# 4 Kleisli categories

```c++
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
```

# 5 Products and coproducts

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
```c++
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
```

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

# 6 Simple algebraic data types

## 1.
Let `f` be the morphism from `Maybe a` to `Either () a` such that:
- `f: x -> Left ()` if x is `Nothing`
- `f: x -> Right x` if x matches `Just a`

Let `g` be the morphism from `Either () a` to `Maybe a` such that:
- `g: x -> Nothing` if x is `Left ()`
- `g: x -> Just x` if x is `Right a`

We have `f . g = g . f = id`, hence `Maybe a` and `Either () a` are isomorphic.

## 2.
```c++
#include <iostream>
#define _PI 3.14159265359

using namespace std;

class Shape {
    public:
    virtual ~Shape() {}
    virtual double area() = 0;
    virtual double circ() = 0;
};

class Circle: public Shape {

    double r;

    public:
    Circle(double r): r(r) {}

    double area() { return _PI * r * r; }
    double circ() { return 2 * _PI * r; }
};

class Rect: public Shape {

    double w, h;

    public:
    Rect(double w, double h): w(w), h(h) {}

    double area() { return w * h; }
    double circ() { return 2 * (w + h); }
};

class Square: public Rect {

    public:
    Square(double s): Rect(s,s) {}
};

int main() {
    Shape* s = new Circle(5);
    cout << s->area() << endl << s->circ() << endl;
    return 0;
}
```

## 3.
To implement `circ` in C++ we need to touch both inheriting classes (and Shape if we want the function to be a requirement), see `algebraic.cc`.

## 4.
If we inherit from `Rect` directly, we have less code to write (and we do not touch anything), but we store the square side length twice in the object.

## 5.
Let `f` be the morphism from `Either a a` (a + a) to `(Bool, a)` (2 * a) such that:
- `f: x -> (True, x)` if x is `Left a`
- `f: x -> (False, x)` if x is `Right a`

Let `g` be the morphism from `(Bool, a)` (2 * a) to `Either a a` (a + a)  such that:
- `g: x -> Left x` if x is `(True, a)`
- `g: x -> Right x` if x is `(False, a)`

We have `f . g = g . f = id`, `Either a a` and `Pair Bool a` are isomorphic. The a + a = 2 * a identity holds for types, up to isomorphism.

# 7 Functors

## 1.
Defining:
```Haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor Maybe where
    fmap _ _ = Nothing
```
breaks the functor laws as `fmap` would not preserve identity, lifting any morphism into one collapsing the image of any object into `Nothing`.

## 2.
```Haskell
instance Functor ((->) r) where
    fmap = (.)
```
We have `fmap id f = id . f = f = id f` and identity is preserved.

Also `fmap (g . h) f = (g . h) . f = g . h . f` by associativity of composition, and then
`fmap (g . h) f = g . (h . f) = fmap g (fmap h f) = (fmap g . fmap h) f`, composition is preserved.

## 3.
```c++
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
```

## 4.
We define:
```Haskell
data List = Nil | Cons a (List a)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x t) = Cons (f x) (fmap f t)
```

We have `fmap id Nil = fmap _ Nil = Nil = id Nil`

Also `fmap (f . g) Nil = fmap _ Nil = Nil = fmap g Nil`

`= fmap f (fmap g Nil) = (fmap f . fmap g) Nil`

Now let us assume that for the tail `t` of the list `Cons x t`, fmap preserves the identity morphism and does not break compostion.

We then have `fmap id (Cons x t) = Cons (id x) (fmap id t)`

`= Cons x (id t) = id (Cons x t)`, fmap preserves identity on lists.

Also `fmap (f . g) (Cons x t) = Cons ((f . g) x) (fmap (f . g) t)`

`= Cons (f (g x)) ((fmap f . fmap g) t)`

`= fmap f (Cons (g x)) ((fmap f . fmap g) t)`

`= (fmap f . fmap g) (Cons x) ((fmap f . fmap g) t)`

`= (fmap f . fmap g) (Cons x t)`, fmap preserves composition on lists.

Our definition of fmap on lists:
- preserves composition and identity given it preserves it for the tail of the list
- preserves composition and identity for the `Nil` value, which is the smallest possible tail.

By induction, fmap preserves identity and composition on lists. The functor laws hold for the list functor.

# 8 Functoriality

## 1.
```Haskell
data Pair a b = Pair a b
```
Having:
```Haskell
instance Bifunctor Pair where
    bimap :: (a -> c) -> (b -> d) -> Pair a b -> Pair c d
    bimap f g (x, y) = (f x, g y)
```

let us show that `bimap` on `Pair` is functorial in `f`, its first argument. For that we prove that the function lifting a morphism applied only on the first element of the `Pair` is indeed a functor.
```Haskell
fmap :: (a -> f a) -> Pair a b -> Pair (f a) b
fmap f (Pair a b) = Pair (f a) b
```
Identity...
```
fmap id (Pair a b) = Pair (id a) b = Pair a b = id (Pair a b)
```
... and composition
```
fmap (f . g) (Pair a b) = Pair ((f . g) a) b
                        = fmap f (Pair (g a) b)
                        = (fmap f . fmap g) (Pair a b)
```
Applying this result to the second argument of `Pair` shows that this data type is a bifunctor.

Moreover we can define:
```Haskell
instance Bifunctor Pair where
    bimap :: (a -> c) -> (b -> d) -> Pair a b -> Pair c d
    bimap f g (Pair a b) = Pair (f a) (g b)

    first :: (a -> c) -> Pair a b -> Pair c b
    first f = bimap f id -- or first f (Pair x y) = Pair (f x) y

    second :: (b -> d) -> Pair a b -> Pair a d
    second = bimap id -- or second f = bimap id f
                      -- or second f (Pair x y) = Pair x (f y)
```
We can check the default implementations for `bimap`, `first` and `second` in definition of the `Control.Bifunctor` typeclass and see they are compatible.

## 2.
We have
```Haskell
data Maybe a = Nothing | Just a
```
and
```Haskell
type Maybe' a = Either (Const () a) (Identity a)
```
Let us define
```Haskell
-- data Either a b = Left a | Right b
f :: Maybe' a -> Maybe a
f (Left (Const () a)) = Nothing
f (Right (Identity a)) = Just a

g :: Maybe a -> Maybe' a
g Nothing = Left (Const ())
g (Just a) = Right (Identity a)
```

We have `(f . g) = (g . f) = id`, we have isomorphism.

## 3.
```Haskell
data PreList a b = Nil | Cons a b
```
Reasoning the same as in 1, we define
```Haskell
fmap :: (a -> f a) -> PreList a b -> PreList (f a) b
fmap f Nil = Nil
fmap f (Cons a b) = Cons (f a) b
```
And now for the usual dance. Identity:
```
fmap id Nil = Nil = id Nil

fmap id (Cons a b) = Cons (id a) b = id (Cons a b)
```
And composition:
```
fmap (f . g) Nil = Nil = (fmap f . fmap g) Nil

fmap (f . g) (Cons a b) = Cons ((f . g) a) b
                        = fmap f (Cons (g a) b)
                        = (fmap f . fmap g) (Cons a b)
```

`Prelist` equipped with this version of `fmap` is functorial in `a`. Hence `PreList` is a bifunctor.

## 4.
```Haskell
data K2 c a b = K2 c
```
Fixing all the type variables except `a` would give us a data type that is trivially isomorphic to `Const`, which is a functor.
Again, holding the same reasoning for `b` shows that `K2` is a bifunctor.

```Haskell
data Fst a b = Fst a
data Snd a b = Snd b
```
Fixing `b` in `Fst` (resp. `a` in `Snd`), gives us a type isomorphic to `Cons a` (resp. `Cons b`) (for "constructor"), which is functorial.
Fixing the other type variable in both cases again yield a type isomorphic to `Const` (for "constant").
`Fst` and `Snd` are bifunctors.

As stated in the book, ["Clowns to the Left of me, Jokers to the Right"](http://strictlypositive.org/CJ.pdf) is a parallel with the notion of derivatives, and introduces polynomial functors.

## 5.
```c++
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
```

## 6.
C++'s `std::map<K, T>` is a profunctor. It is contravariant in `K` and covariant in `T`.
```Haskell
std_map k t :: k -> t -- better with k -> Maybe t, handling unmapped keys

instance Profunctor std_map where
    dimap f g std_map = f . std_map . g
    lmap f = flip (.) -- "reversing the arrow"
    rmap g = (.)
```

# 10 Natural transformations

## 1.
We define the `alpha` natural transformation as follows, for every object `a` in `Hask`:
```Haskell
alpha :: Maybe a -> [a]
alpha Nothing = []
alpha (Just x) = [x]
```

Let `a` and `f a` be two objects, connected by a morphism `f`. Let us denote `fmapM` the implementation of `fmap` for the `Maybe` functor and `fmapL` the one for `List`.
```Haskell
alpha (fmapM f Nothing) = alpha Nothing = []
alpha (fmapM f Just(x)) = alpha Just(f x) = [f x]

fmapL f (alpha Nothing) = fmapL f [] = []
fmapL f (alpha Just (x)) = fmapL f [x] = [f x]
```

We have `fmapL . alpha = alpha . fmapM`, the naturality condition holds for `alpha`.

## 2.
10.1 gives us two natural transformations between the `Reader ()` and `Maybe` functors:
```Haskell
dumb (Reader _) = Nothing

obvious (Reader g) = Just (g ())
```

We just defined `alpha` going from `Maybe` to `List`, so we
have, by virtue of vertical composition, at least one natural transformation from `Reader ()` to `List`. In order to affirm those compositions are indeed
two different natural transformations, we need to realize that there is no way a `Just` is equal to a `Nothing`, and therefore there is no `g` for
which `obvious (Reader g)` is equal to `dumb (Reader g)`. As there exists at least one value of `obvious` and `dumb`, in particular there exists one g
for which they are different, which is enough to prove the compositions are different.

We could easily tweak our `alpha` so that `alpha Just (f x) = [f x, f x]` or any number of repetitions of `f x`. We would then have an infinity of
natural transformations between `Reader ()` to `List`. By the Yoneda lemma, there is a one to one correspondence between those functors and the elements
of `List ()`. Therefore, there is an infinite number of values of the type `List ()`. Which makes sense as we can just keep appending `()` to `[()]`.

## 3.
```Haskell
alpha :: Reader Bool a -> Maybe a
alpha (Reader _) = Nothing

beta :: Reader Bool a -> Maybe a
beta (Reader g) = Just (g True)
```

## 4.
Let us define four functors `F, G, F', G'`, two objects `a, b`, one morphism `f :: a -> b` and
two natural transformations `alpha` and `beta` such that:
```Haskell
alpha_a :: F a -> F' a
alpha_b :: F b -> F' b

beta_a :: G a -> G' a
beta_b :: G b -> G' b

-- In essence:
-- "alpha F = F'"
-- "beta G = G'"
```

The horizontal composition of `beta` and `alpha` is a morphism `(beta ◦ alpha) :: G . F -> G' . F'`. Let us prove the naturality condition for the horizontal composition of `alpha` and `beta`:
```
(beta ◦ alpha)_b . (G . F) f = (beta_F'b . G_alpha_b) . G . F f

                             = beta_F'b . G . F'f . alpha_a

                             = (G' . F') . (beta ◦ alpha)_a
```
And the naturality condition holds for `(beta ◦ alpha)`.

## 5.
A diagram for the proof of the interchange law on horisontal composition of natural transformations would be similar to the third of the 10.4 section, except we need to introduce
two more functors (destinations for `alpha'` and `beta'`). We would then, as said Saunders Mac Lane, have a very good time putting colors on our edges to show the paths are equivalent.

## 6.
Let us define, for instance:
```Haskell
newtype Op r a = Op (a -> r)
instance Contravariant Op r where
    contramap f (Op g) = Op (g . f)

op :: Op Bool Int
op = Op (\x -> x > 0)

f :: String -> Int
f x = read x

alpha :: Op Bool a -> Op String a
alpha (Op f) = Op (\x -> if f x "T" else "F")
```

We have:
```
contramap f . alpha (op)
    = contramap f (Op (\x -> if x > 0 then "T" else "F"))
    = Op (\x -> if (read x) > 0 then "T" else "F")
    = alpha (Op (\x -> (read x) > 0))
    = alpha . contramap f (Op (\x -> x > 0))
    = alpha . contramap f (op)
```

# 12 Limits and colimits

## 1.
Let Cpp be the category of C++ classes, where arrows connect subclasses to superclasses (inheritance is considered transitive to have composability).

Given two classes A and B, and a class C inheriting from both, we have the following morphisms in Cpp:
```Haskell
p :: C -> A
q :: C -> B
```
A pushout would be a limit: it would be the "most general" object P closing the commuting diagram formed by P, A, B, C, p and q (introducing two new morphisms `i1` and `i2`, from A (resp. B) to P)).
By "general", we mean `i1` and `i2` factorize every other candidate for P.

That would correspond to the superclass S such that:
- A and B (and C by transitivity / composition) inherit from S.
- S inherits from any class A and B inherit from.

## 2.
The limit of the Id endofunctor would be an object a such that from every other "apex" candidate for the cone diagram, there is a morphism from a to it. Since the initial object
admits morphisms to every object, in particular it does work. By unicity of the limit, a is the initial object.

## 3.
In such a category, a pullback of two objects would be the intersection of two sets.
A pushout would be their union.

The initial object in this category would be the set containing all the others (it exists since the category is built with subsets of a given set).
The terminal object would be the empty set (since it is a subset of any set, we know it is in the category).

## 4.
As an equalizer is a limit, we can guess its dual the coequalizer is a colimit.

By reversing the arrows in the figure in 12.2, we can guess a coequalizer for two parallel morphisms `f`
and `g` from `A` to `B` we have a projection `p` going from `B` to `C`, that builds a commuting
co-cone of apex `C`.

This means `p . f = p . g`, and for all `q` also closing the co-cone such that `q . f = q . g`
there exists a unique morphism `m` such that `q = m . p`.

[This article on nLab](https://ncatlab.org/nlab/show/coequalizer) defines a coequalizer in
terms of equivalence relations and links to the notion of quotient sets.

## 5.
A pullback "towards" an object `B` is a pullback on the cospan `A -> B <- C`.

If we define `f :: A -> B` and `g :: C -> B` such that `B` is the terminal object, as `f` and `g` are unique they can be omitted from the diagram. What is left is a product construction.
The apex `p` of the pullback cone is the product of `A` and `C`.

## 6.
In the same way, reversing the arrows on a pullback diagram, and if `B` is initial, the morphisms `f :: B -> A` and `g :: B -> C` are the only ones from `B` to `A` (resp. `C`) and
do not add constraints on the diagram other than the ones satisfied by a coproduct.

# 13 Free monoids

## 1.
Let `m` be a monoid and `f :: m -> f_m` an isomorphism (`<=> ∃ f⁻¹, f . f⁻¹ = f⁻¹ . f = Id`).
Let `u_m` and `u_f_m` be units in `m` and `f_m`, respectively.

We have:
```
∀ a ∈ m, f u_m * f a = f (u_m * a)
                      = f a
```
As f is an isomorphism, the image of `m` by f entirely covers `f_m`. We have established this identity for all `f a` in `f_m`, hence `f u_m = u_f_m`, QED.

## 2.
Let `h` be said homomorphism. As such it preserves unit and maps `[]` to the neutral of the monoid formed by integers equipped with multiplication, which is 1.

By virtue of `h` preserving multiplication, and `∀ a, h [a] = a` we have:
```
h [1, 2, 3, 4] = h ([1] * [2] * [3] * [4])
               = h [1] * h [2] * h [3] * h [4]
               = 24
```
Note that on line 1, * represents list concatenation, whereas on line 2 it represents integer multiplication.

There are infinitely many lists mapping to every integer as we can keep adding 1 to any list. Moreover in the case of 0, concatenating any list with `[0]` will collapse its image under `h` on 0.

We could define a morphism `h'` that would map any list of integers `L` to `h ([1] * L)`, where * represents list concatenation. This effectively yields an homomorphism similar to `h`,
and we can repeat this pattern to create infinitely many homomorphisms between our two monoids.

## 3.
Such a monoid would consist of all sequences of its unique generator `g`: namely `g; (g, g); (g, g, g)...`. Note that the unit element comes with the composition law and is not part of the generators.
We identified all the "tuples" containing `e` here with the tuple containing the corresponding number of `g`s.
By substituting composition with addition, `g` with 1 and `e` with 0, we can see our free monoid is isomorphic to ℕ.

# 14 Representable functors

## 1.
Functor laws state that when functors lift morphisms from a category C1 to another category C2, they preserve composition and identity. The hom-functor from any category C to Set maps any
identity morhpism in C to the identity function in Set.

## 2.
Showing that a functor F is representable is equivalent to providing an invertible natural transformation between F and the hom-functor. That is, two naural transformations `alpha` and
`beta` such that, for any given a in a category C:

```Haskell
{-# LANGUAGE ExplicitForAll #-}

alpha :: forall x. (a -> x) -> F x -- The hom-functor is Reader

beta :: forall x. F x -> (a -> x)

alpha . beta = beta . alpha = id
```

Let us show that, if F is the `Maybe` functor, such a pair or morphisms cannot exist.
While choosing `alpha` is pretty straightforward ( `alpha f a = Just (f a)` ), defining `beta` requires us to provide a morphism from `Maybe x` to `(a -> x)`. Mapping `Nothing` to a morphism
from a to x implies creating an x out of thin air. The `Maybe` functor is not representable.

## 3.
The Reader functor is by definition equivalent to the hom-functor: it represents morphisms from an object to another in a given category. Therefore, it is representable (the isomorphic
natural transformation is identity).

## 4.
```Haskell
-- Assuming we have

data Stream a = Cons a (Stream a)

class Representable f where
    type Rep f :: *
    tabulate :: (Rep f -> x) -> f x
    index :: f x -> Rep f -> x

instance Representable Stream where
    type Rep Stream = Integer
    tabulate f = Cons (f 0) (tabulate (f . (+1)))
    index (Cons b bs) n = if n == 0 then b else index bs (n-1)

-- The we can memoize f into a Stream of integers

f :: Int -> Int
f x = x * x

memoizedF = tabulate f
```

`index memoizedF sq` where sq is the square of an integer n will yield n from the Stream, memoizing values up to sq on the fly.

## 5.
```Haskell
s :: Stream b
s = Cons b bs

tabulate (index s) = Cons (index s 0) (tabulate ((indxe s) . (+1)))
```

We see by pattern matching the obtained stream that its head `index s 0` is the head of our stream s. In summary, for every stream, `tabulate . index` is a morphism preseving its head,
the b in `Cons b bs`. In this form `bs` is itself a stream. By induction, `tabulate . index` preserves the stream for any stream: it is the identity morphism. `tabulate` and `index` are inverse of each other.

## 6.
```Haskell
instance Representable Pair where
    type Rep Pair = Bool
    tabulate f = Pair (f True) (f False)
    index (Pair a1 a2) i = if i then a1 else a2
```
