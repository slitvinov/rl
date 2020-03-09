#!/usr/bin/python

import scipy.optimize
import scipy
import numpy
import math

x0 = 1
M = 200
N = 1

a = 0.1
b = 0.4

class Tab(object):
    def __init__(self, x, y):
        self.x = x
        self.y = y
    def f(self, x):
        return numpy.interp(x, self.x, self.y)

class Inv(object):
    def __init__(self, f):
        self.f = f
    def __call__(self, *arg):
        return -self.f(*arg)

def g(y):
    return - y ** 2
def h(y):
    return - (y  - 0.5) ** 2

def fun0(y, x):
    return  g(y) + h(x - y)

def fun(y, x, t):
    return g(y) + h(x - y) + t.f(a*y + b*(x - y))

def maximize0(f, x, *args):
    y0 = x/2
    args = (x,) + args
    res = scipy.optimize.minimize(Inv(f), y0, args = args, bounds = ((0, x),))
    return res.x[0], -res.fun[0]

def maximize(fu, *args):
    xx = []
    yy = []
    ff = []
    step = x0/(M - 1)
    for i in range(M):
        x = i * step
        y, f = maximize0(fu, x, *args)
        xx.append(x)
        ff.append(f)
        yy.append(y)
    return Tab(xx, ff), Tab(xx, yy)

f, y = maximize(fun0)
yy = [y]
for i in range(N-1):
    f, y = maximize(fun, f)
    yy.append(y)
yy.reverse()

x = x0
i = 0
while True:
    yn = y.f(x)
    print(yn)
    if i == N - 1:
        break
    x = fun0(yn, x)
    i += 1
