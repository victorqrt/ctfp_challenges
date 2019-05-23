# 10.6 Challenges

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
