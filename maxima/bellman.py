#!/usr/bin/python

import scipy.optimize
import scipy
import numpy
import math

X = 1
M = 10
N = 6

a = 0.1
b = 0.4

def g(y):
    return math.sin(y)
def h(y):
    return math.cos(y)

def fun0(y, x):
    return g(y) + h(x - y)

def fn(y, t):
    return numpy.interp(y, *t)[0]

def fun(y, x, t):
    return g(y) + h(x - y) + fn(a*y + b*(x - y), t)

def maximize0(f, x, *args):
    y0 = x/2
    args = (x,) + args
    res = scipy.optimize.minimize(f, y0, args = args, bounds = ((0, x),))
    return res.x, res.fun

def maximize(f, *args):
    xx = []
    yy = []
    step = X/(M - 1)
    for i in range(M):
        x = i * step
        y = maximize0(f, x, *args)[1]
        xx.append(x)
        yy.append(y)
    return xx, yy

t = maximize(fun0)
tt = [t]
for i in range(N):
    t = maximize(fun, t)
    tt.append(t)

tt.reverse()    
