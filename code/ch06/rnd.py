import random
import math
import itertools
import sys

random.seed(10)
n = 5
global alpha
global lmbd
half = 0.5
w_ini = tuple([half] * n)
ref = [(i + 1)/(n + 1) for i in range(n)]
eps = .000000001

def step():
    return 1 if random.uniform(0, 1) > 0.5 else -1

def gen_walks(nset, nwalk):
    return [[walk() for i in range(nwalk)] for j in range(nset)]

def process0(walk, w, dw):
    global lmbd
    outcome, states = walk
    n = len(states)
    for t in range(n):
        P1 = w[states[t + 1]] if t + 1 < n else outcome
        P = w[states[t]]
        j = states[t]
        dw[j] += P1 - P
        for k in range(t):
            j = states[k]
            dw[j] += (P1 - P) * lmbd**(t - k)

def process1(wset, w):
    w = list(w)
    cnt = 0
    while True:
        cnt += 1
        w0 = w[:]
        for walk in wset:
            dw = [ 0 ] * n
            process0(walk, w, dw)
            for i in range(len(w)):
                w[i] += alpha * dw[i]
            if not good(w):
                w = [max(0, e) for e in w]
                w = [min(1, e) for e in w]
                sys.stderr.write("w = %s\n" % str(w))
                #break
        break
        e = diff(w, w0)
        if e < eps:
            break
        if cnt % 100000 == 0:
            sys.stderr.write("cnt = %d, e = %g\n" % (cnt, e))
            sys.stderr.write("dw = %s\n" % str(dw))
            sys.stderr.write("w = %s\n" % str(w))
            sys.stderr.write("wset = %s\n" % str(wset))
    #sys.stderr.write("w = %s (%.3f, %d)\n" % (fmt("%4.2f", w), rmse(ref, w), cnt))
    return w

def process(nset, nwalk):
    err = 0
    n = len(w_ini)
    ws = 0
    for s in gen_walks(nset, nwalk):
        w = process1(s, w_ini)
        ws += ssq(w, ref)
    return math.sqrt(ws/len(w)/nset)

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
    s = sum( (x - y)**2 for x, y in zip(a, b))
    return math.sqrt(s/len(a))

def ssq(a, b):
    return sum((x - y)**2 for x, y in zip(a, b))

def sim():
    global alpha
    al = [5*i/100 for i in range(13)]
    ans = []
    for alpha in al:
        e = process(100, 10)
        print(e)
        ans.append(e)
    return alpha, ans

def good(w):
    return 0 <= min(w) and max(w) <= 1

def fmt(f, l):
    l = [f % e for e in l]
    return ' '.join(l)
