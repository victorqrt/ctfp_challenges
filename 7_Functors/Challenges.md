# 7.4 Challenges

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
See `reader.cc`.

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