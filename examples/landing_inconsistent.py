from z3 import *

ALPHA = 30
BETA = 10
MAX_PTRACE = 10

handle_down = Bool('handle_down')
gear_extended = Bool('gear_extended')
gear_retracted = Bool('gear_retracted')
door_closed = Bool('door_closed')
door_open = Bool('door_open')

REQ_SET = [
    # If the command handle is pushed down and kept down then, 
    # the landing gears must be extended and the door of the landing gear boxes must be closed
    # within 5 seconds.
    [ 
		And( handle_down, Not(And(door_closed, gear_extended)) ), 
		And( handle_down, Not(And(door_closed, gear_extended)) ), True, 
		5, 5, 0, 0, And(door_closed, gear_extended), True, True, 0, 0
	],
	# The door of the landing gear boxes must not be open and closed at the same time.
	[
		True, True, True, 0, 0, 1, 1, Not( And(door_open, door_closed) ), True, True, 0, 0
	],
	# The landing gears must not be extended and retracted at the same time.
	[
		True, True, True, 0, 0, 1, 1, Not( And(gear_extended, gear_retracted) ), True, True, 0, 0
	],
	# If the landing gears are neither extended nor retracted (intermediate state) then door must be open.
	# (!gear_extend & !gear_retracted) -> door_open
	# !(!gear_extend & !gear_retracted) | door_open
	# gear_extend | gear_retracted | door_open
	[
		True, True, True, 0, 0, 1, 1, Or( door_open, gear_extended, gear_retracted ), True, True, 0, 0
	],
	# If the landing gear is retracted and the door is not open then,
	# the landing gear must remain retracted in the next time step.
	[
		And( gear_retracted, Not(door_open) ), True, True, 0, 0, 1, 1, gear_retracted, True, True, 0, 0
	],
	# If the landing gear is extended and the door is not open then,
	# the landing gear must remain extended in the next time step.
	[
		And( gear_extended, Not(door_open) ), True, True, 0, 0, 1, 1, gear_extended, True, True, 0, 0
	],
	# If the landing gear is retracted and the door is open then,
	# the landing gear must remain retracted or the door must remain open in the next time step.
	[
		And( gear_retracted, door_open ), True, True, 0, 0, 1, 1, Or(gear_retracted, door_open), True, True, 0, 0
	],
	# If the landing gear is extended and the door is open then,
	# the landing gear must remain extended or the door must remain open in the next time step.
	[
		And( gear_extended, door_open ), True, True, 0, 0, 1, 1, Or(gear_extended, door_open), True, True, 0, 0
	],
	# If the command handle is not pushed down and the landing gear is retracted then,
	# the landing gear must remain retracted in the next time step.
	[ 
		And( Not(handle_down), gear_retracted ), True, True, 0, 0, 1, 1, gear_retracted, True, True, 0, 0 
	],
	# If the command handle is pushed down and the landing sequence is not completed then, 
	# the door of the landing gear boxes must be open within 3 seconds and
	# must remain open until the landing gear is extended.
	# The landing gear must be extended within 5 seconds of the door opening.
	# If the command handle leaves the position, the above conditions need not be satisfied.
	[
		And( handle_down, Not( And(gear_extended, door_closed) ) ), True, True, 0, 0, 0, 3, 
		Or( door_open, Not(handle_down) ), Or( door_open, Not(handle_down) ), 
		Or( gear_extended, Not(handle_down) ), 0, 5
	],
	# To be generated for resolving an inconsistency
	# If the command handle is pushed down and the landing sequence is not completed then, 
	# the landing gear must be extended within 4 seconds and
	# must remain extended until the doors of the landing gear boxes are closed.
	# The door must be closed within 5 seconds of the landing gear extending.
	# If the command handle leaves the position, the above conditions need not be satisfied.
	# [
	# 	And( handle_down, Not( And(gear_extended, door_closed) ) ), True, True, 0, 0, 0, 4, 
	# 	Or( gear_extended, Not(handle_down) ), Or( gear_extended, Not(handle_down) ), 
	# 	Or( door_closed, Not(handle_down) ), 0, 5
	# ]
	[
		Or( And(handle_down, Not(gear_extended)), And(handle_down, Not(door_closed)) ), 
		True, True, 0, 0, 0, 4, 
		#gear_extended, gear_extended, door_closed, 0, 1
		gear_extended, True, door_closed, 1, 1
	]
]

COND_INIT = [ 
	handle_down == False, gear_extended == False, gear_retracted == True, 
	door_closed == True, door_open == False
]
