# 12.5 Challenges

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
