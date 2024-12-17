from z3 import *

# Specify a first upper bound of bounded partial consistency
ALPHA = 30

# Specify a second upper bound of bounded partial consistency
BETA = 10

# Specify a max length of positive traces for requirement improvement
MAX_PTRACE = 10

on = Bool('on')
blink = Bool('blink')
REQ_SET = [
    [ on , True , True , 0, 0, 0, 0, True, on, Not(on), 10, 10],
    [ And(blink, Not(on)), Not(on), True, 10, 10, 0, 0, True, True, on, 0, 0]
]

COND_INIT = []