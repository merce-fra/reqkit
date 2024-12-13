from z3 import *

# This variant is rt-consistent because blink can be false at t=3 for req 2

ALPHA = 30
BETA = 10
MAX_PTRACE = 10

on = Bool('on')
blink = Bool('blink')
low = Bool('low')
REQ_SET = [
    [ on , True , True , 0, 0, 0, 0, True, on, Not(on), 10, 10],
    [ Not(on), Not(on), blink, 10, 10, 0, 0, True, True, on, 0, 0],
    [ low, True, True, 0, 0, 0, 0, True, Not(on), True, 20, 20]
]

COND_INIT = []