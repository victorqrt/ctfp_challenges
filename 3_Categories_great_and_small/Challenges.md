# 3.6 Challenges

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
