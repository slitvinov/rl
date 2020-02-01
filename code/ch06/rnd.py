import random
n = 4
lamb = 0.9
alpha = 0.1
w = [0.5] * n

def step():
    return 1 if random.uniform(0, 1) > 0.5 else -1

def gen_walks(nset, nwalk):
    return [[walk() for i in range(nwalk)] for j in range(nset)]

def learn(x, target):
    print(x, target)

def process(walk):
    outcome, states = walk
     for s1, s2 in two(states):
        learn(s1, w[s2])
    learn(s2, outcome)

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

def two(l):
    l = iter(l)
    x = next(l)
    for y in l:
        yield x, y
        x = y
