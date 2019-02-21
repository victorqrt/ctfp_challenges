# 6.5 Challenges

## 1.
Let `f` be the morphism from `Maybe a` to `Either () a` such that:
- `f: x -> Left ()` if x is `Nothing`
- `f: x -> Right a` if x matches `Just a`

Let `g` be the morphism from `Either () a` to `Maybe a` such that:
- `g: x -> Nothing` if x is `Left ()`
- `g: x -> Just a` if x is `Right a`

We have `f . g = g . f = id`, hence `Maybe a` and `Either () a` are isomorphic.
