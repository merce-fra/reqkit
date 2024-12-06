from z3 import *

# Specify a first upper bound of bounded partial consistency
ALPHA = 30

# Specify a second upper bound of bounded partial consistency
BETA = 15

# Specify a max length of positive traces for requirement improvement
MAX_PTRACE = 15

# a work is present on the carriage
X1 = Bool('x1')
# the carriage is in the forward limit of the conveyor
X2 = Bool('x2')
# the carriage is in the backward limit of the conveyor
X3 = Bool('x3')
# the carriage is moving forward
Y71 = Bool('y71')
# the carriage is moving backward
Y72 = Bool('y72')
# the arm is pushing a work
Y73 = Bool('y73')
# the timer is expired
T0 = Bool('t0')

# Specify a set of requirements
# Order of SUP parameters is as following: 
# [ TSE, TC, TEE, Tmin, Tmax, Lmin, Lmax, ASE, AC, AEE, Amin, Amax ]
REQ_SET = [
    # If a work is present and the carriage is at the backward limit then, 
    # the carriage must move forward to the forward limit.
    [
        And( X1, X3 ), True, True, 0, 0, 1, 1, Y71, Y71, And( Y71, X2 ), 0, -1
    ],
    # If a work is not present or the carriage is not at the backward limit, 
    # and the carriage is not moving forward then,
    # the carriage must not move forward.
    [
        And( Or( Not(X1), Not(X3) ), Not(Y71) ), True, True, 0, 0, 1, 1, Not(Y71), True, True, 0, 0
    ],
    # If the carriage is moving forward and reaches to the forward limit,
    # the carriage must stop and the arm must push the work until the timer expires.
    [
        And( Y71, X1, X2 ), True, True, 0, 0, 1, 1, And( Not(Y71), Y73 ), Y73, T0, 0, -1
    ],
    # If the carriage is not moving forward or does not reach to the forward limit,
    # and the arm is not pushing the work then,
    # the arm must not move.
    [
        And( Or( Not(Y71), Not(X1), Not(X2) ), Not(Y73) ), True, True, 0, 0, 1, 1, Not(Y73), True, True, 0, 0
    ],
    # If the timer does not expire and the arm pushes the work for 5 seconds then,
    # the timer must expire until the arm stops.
    [
        And( Y73, Not(T0) ), Y73, Y73, 4, 4, 1, 1, T0, T0, Not(Y73), 0, -1
    ],
    # If the arm is not moving then the timer must not expire.
    [
        Not(Y73), True, True, 0, 0, 0, 0, Not(T0), Not(T0), Not(T0), 1, 1 
    ],
    # If the arm is pushing the work and the timer does not expire then, 
    # the timer must not expire for 5 seconds.
    [
        And( Y73, Not(T0) ), True, True, 0, 0, 1, 1, Not(T0), Not(T0), True, 4, 4
    ],
    # If the time expires then, 
    # the arm must stop and the carriage must move back to the backward limit.
    [
        T0, True, True, 0, 0, 1, 1, And( Not(Y73), Y72 ), Y72, And( Y72, X3 ), 0, -1
    ],
    # If the timer does not expire and the carriage is not moving backward then,
    # the carriage must not move backward.
    [
        And( Not(T0), Not(Y72) ), True, True, 0, 0, 1, 1, Not(Y72), True, True, 0, 0
    ],
    # If the carriage is moving backward and reaches to the backward limit then,
    # the carriage must stop.
    [
        And( Y72, X3 ), True, True, 0, 0, 1, 1, Not(Y72), True, True, 0, 0
    ],
    # The carriage must not be in the forward and backward limit at the same time.
    [
        True, True, True, 0, 0, 1, 1, Or( Not(X2), Not(X3) ), True, True, 0, 0
    ],
    # If the carriage is in the backward limit then,
    # the carriage must not be in the forward limit until moving forward.
    [
        X3, True, True, 0, 0, 1, 1, Not(X2), Not(X2), And( Not(X2), Y71 ), 0, -1
    ],
    # Missing requirement (to be generated for resolving an inconsistency)
    # If the carriage is in the forward limit then,
    # the carriage must not be in the backward limit until moving backward.
    [
    #    X2, True, True, 0, 0, 1, 1, Not(X3), Not(X3), And( Not(X3), Y72 ), 0, -1
        X2, True, True, 0, 0, 1, 1, Not(X3), Not(X3), True, 0, 20
    ]
]

# Specify an initial value of each variable
COND_INIT = [ 
    X1 == False, X2 == False, X3 == True,
    Y71 == False, Y72 == False, Y73 == False,
    T0 == False
]














