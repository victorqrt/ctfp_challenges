# 13.3 Challenges

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