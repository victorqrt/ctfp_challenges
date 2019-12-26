# 15.3 Challenges

## 1.
```Haskell
{-# LANGUAGE ExplicitForAll #-}

phi :: (forall x . (a -> x) -> F x) -> F a
phi alpha = alpha id

psi :: F a -> (forall x . (a -> x) -> F x)
psi fa h = fmap h fa
```

We have:

```
(phi . psi) fa = phi (\h -> fmap h fa)
               = (\h -> fmap h fa) $ id
               = fmap id fa
               = fa                     (By functor laws)
```

and

```
(psi . phi) alpha = psi (alpha id)
                  = \h -> fmap h (alpha id)
                  = \h -> alpha (fmap h id)
                  = \h -> alpha h
                  = alpha
```

`(phi . psi) = (psi . phi) = id`, QED.

## 2.

Let `D` be a discrete category, and `a` an object in `D`.
The hom-functor `D(a, -)` maps every object in `D` to the empty set, excepted for `a` itself, which is mapped to a singleton set with only `id_a` in it.

Let us define a natural transformation `alpha` from `D(a, -)` to `F`.
Component-wise, for every object `x` other than `a`, `alpha_x` is equal to `absurd`: it maps `D(a, x)` (the empty set) to `F x`.
By definition of the initial object in `Set`, `absurd` is the only candidate for all of those components.

This means that `alpha` is entirely determined by its component at `a`.
This component is a morphism between two sets:

```Haskell
alpha_a :: (a -> a) -> F a
```

The domain of this morphism is a singleton:  it is entirely determined by the value we pick in `F a` for `alpha_a id_a`.

In other terms, `alpha` only depends on its component in `a`, which itself only depends on the choice of an element in `F a`.

There is a one-to-one correspondance between natural transformations `D(a, -) -> F` and elements of `F a`.
This is exactly the Yoneda lemma.

## 3.

Let `Hask((), -)` be the hom-functor for `()` in the category of Haskell types.
We can also write it as `Reader ()` or `forall a . () -> a`.

By the Yoneda lemma, we have a one-to-one mapping between all `alpha :: (Reader ()) -> F` and the elements of `F ()` for any functor `F` from `Hask`.

In particular `List` is a functor. So we can represent `List ()` using the type `(Reader ()) -> List`.
