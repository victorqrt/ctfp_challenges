# 8.9 Challenges

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
See `bifunctor.cc`.

## 6.
C++'s `std::map<K, T>` is a profunctor. It is contravariant in `K` and covariant in `T`.
```Haskell
std_map k t :: k -> t -- better with k -> Maybe t, handling unmapped keys

instance Profunctor std_map where
    dimap f g std_map = f . std_map . g
    lmap f = flip (.) -- "reversing the arrow"
    rmap g = (.)
```