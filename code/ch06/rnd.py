import random
import math
import itertools
import sys

n = 5
lmbd = 0.0
global alpha
w_ini = tuple([0.5] * n)
ref = [(i + 1)/(n + 1) for i in range(n)]
eps = 0.0000001

def step():
    return 1 if random.uniform(0, 1) > 0.5 else -1

def gen_walks(nset, nwalk):
    return [[walk() for i in range(nwalk)] for j in range(nset)]

def process0(walk, w, dw):
    outcome, states = walk
    n = len(states)
    for t in range(n):
        for k in range(t + 1):
            P1 = w[states[t + 1]] if t + 1 < n else outcome
            P = w[states[t]]
            j = states[k]
            dw[j] += (P1 - P) * lmbd**(t - k)

def process1(wset, w):
    w = list(w)
    while True:
        w0 = w[:]
        dw = [ 0 ] * n
        for walk in wset:
            process0(walk, w, dw)
        for i in range(n):
            w[i] += alpha * dw[i]/len(wset)
        w = [max(0, e) for e in w]
        w = [min(1, e) for e in w]
        e = diff(w, w0)
        if e < eps:
            break
        if e > 10:
            sys.stderr.write("w = %s\n" % str(w))
            sys.stderr.write("e = %g\n" % e)
            break
    return w

def process(nset, nwalk):
    err = 0
    for s in gen_walks(nset, nwalk):
        w = process1(s, w_ini)
        err += rmse(ref, w)
    return err/nset

def walk():
    x = n // 2
    s = [ ]
    while x >= 0 and x < n:
        s.append(x)
        x += step()
    if x < 0:
        return 0, s
    else:
        return 1, s

def diff(a, b):
    s = sum(abs(x - y) for x, y in zip(a, b))
    return s

def rmse(a, b):
    n = len(a)
    s = sum( (x - y)**2 for x, y in zip(a, b))
    return math.sqrt(s/len(a))

def sim():
    global alpha
    al = [0.05*i for i in range(13)]
    ans = []
    for alpha in al:
        e = process(100, 10)
        print(e)
        ans.append(e)
    return alpha, ans
    
