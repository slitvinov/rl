#######################################################################
# Copyright (C)                                                       #
# 2016-2018 Shangtong Zhang(zhangshangtong.cpp@gmail.com)             #
# 2016 Kenta Shimada(hyperkentakun@gmail.com)                         #
# Permission given to modify the code as long as you keep this        #
# declaration at the top                                              #
#######################################################################
import numpy as np

def diff(a, b):
    d = [abs(x - y) for x, y in zip(a, b)]
    return max(d)

GOAL = 100
STATES = list(range(GOAL + 1))
HEAD_PROB = 0.4

v = [0] * (GOAL + 1)
v[GOAL] = 1.0

h = []
while True:
    old_v = v.copy()
    for state in STATES[1:GOAL]:
        a = range(min(state, GOAL - state) + 1)
        action_returns = []
        for action in a:
            action_returns.append(
                HEAD_PROB * v[state + action] + (1 - HEAD_PROB) * v[state - action])
        new_value = max(action_returns)
        v[state] = new_value
    delta = diff(v, old_v)
    if delta < 1e-9:
        break

policy = [0] * (GOAL + 1)
for state in STATES[1:GOAL]:
    a = range(min(state, GOAL - state) + 1)
    action_returns = []
    for action in a:
        action_returns.append(
            HEAD_PROB * v[state + action] + (1 - HEAD_PROB) * v[state - action])
    policy[state] = a[np.argmax(np.round(action_returns[1:], 5)) + 1]

for p in policy:
    print(p)
