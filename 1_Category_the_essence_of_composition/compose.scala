#!/bin/bash
exec scala "$0" "$@"
!#

def identity[T <: Any](x: T): T = x

def compose[T <: Any](f: T => T, g: T => T): T => T = (x: T) => g(f(x))

def e(x: Int): Int = x + 1

require(compose(identity[Int], e)(5) == e(5))

// 4. If we consider objects as web pages and morphisms as <a> links, then the www
//    is not a category, as there exists a pair of morphisms such as their composition
//    does not exist: a page may link you to another web page, but does not contain all
//    the links the latter contains.

// 5. No, because of the same problem: if we consider friendships as morphisms, being
//    friend with someone does not imply being friend with its friends as well; no composablity.

// 6. A directed graph is a category if and only if for each path between 3 nodes a->b->c there
//    is another edge going from a to c.
