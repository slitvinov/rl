#!/usr/bin/python

import scipy.optimize
import scipy

X = 1
M = 100
N = 5

a = 0.1
b = 0.4

def g(y):
    return y
def h(y):
    return y

def fun0(y, x):
    return g(y) + h(x - y)

def fun(y, x, fn):
    return g(y) + h(x - y) + fn(a*y + b*(x - y))

def maximize0(f, x, *args):
    y0 = x/2
    args = (x,) + args
    res = scipy.optimize.minimize(f, x0, args = args, bounds = ((0, x),))
    return res.x[0], res.fun[0]

def maximize(f, *args):
    xx = []
    yy = []
    step = X/(M - 1)
    for i in range(M):
        x = i * step
        y = maximize0(f, x, *args)[1]
        xx.append(x)
        yy.append(y)
    return lambda y: numpy.interp(y, xx, yy)

f = maximize(fun0)
ff = [f]
for i in range(N):
    f = maximize(fun, f)
    ff.append(f)

ff.reverse()    
