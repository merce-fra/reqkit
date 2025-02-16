# Copyright 2025 Mitsubishi Electric R&D Centre Europe
# Author: Reiya Noguchi
# Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
# 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
# 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
from z3 import *

# Specify a first upper bound of bounded partial consistency
ALPHA = 30

# Specify a second upper bound of bounded partial consistency
BETA = 10

# Specify a max length of positive traces for requirement improvement
MAX_PTRACE = 10

handle_down = Bool('handle_down')
#handle_up = Bool('handle_up')
gear_extended = Bool('gear_extended')
gear_retracted = Bool('gear_retracted')
door_closed = Bool('door_closed')
door_open = Bool('door_open')
#open_EV = Bool('open_EV')
#close_EV = Bool('close_EV')
#extend_EV = Bool('extend_EV')
#retract_EV = Bool('retract_EV')

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
	[
		And( handle_down, Not( And(gear_extended, door_closed) ) ), True, True, 0, 0, 0, 4, 
		Or( gear_extended, Not(handle_down) ), Or( gear_extended, Not(handle_down) ), 
		Or( door_closed, Not(handle_down) ), 0, 5
	]
	# [
	# 	Or( And(handle_down, Not(gear_extended)), And(handle_down, Not(door_closed)) ), 
	# 	True, True, 0, 0, 0, 4, 
	# 	#gear_extended, gear_extended, door_closed, 0, 1
	# 	gear_extended, True, door_closed, 1, 1
	# ]
]

COND_INIT = [ 
	handle_down == False, gear_extended == False, gear_retracted == True, 
	door_closed == True, door_open == False
]
