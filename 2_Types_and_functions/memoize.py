#!/usr/bin/env python

def memoize(f):
    memtable = {}
    def f2(x):
        if x not in memtable:
            memtable[x] = f(x)
        return memtable[x]

    return f2

import time
@memoize
def compute(x):
    time.sleep(1)
    return x

print(compute(3))
print(compute(3))
