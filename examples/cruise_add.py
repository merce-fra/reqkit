from z3 import *

# Specify a first upper bound of bounded partial consistency
ALPHA = 30

# Specify a second upper bound of bounded partial consistency
BETA = 10

# Specify a max length of positive traces for requirement improvement
MAX_PTRACE = 10

"""
Inputs:
-------
- Lever positions: backward (deactivate), forward (activate),
                   up5, up7 (increase target speed), down5, down7 (decrease target speed)
- brakePedal, gasPedal

Additional Boolean state vars:
- speed>20 (speed is more than 20), prevDesiredSpeed (desired speed was previously given),
- cruiseControlActivated, deactiveCruiseControl
- targetSpeedIncrement (by 1), targetSpeedIncrement10 (by 10)
"""

forward = Bool('forward')
backward = Bool('backward')
# down5 = Bool('down5')
# down7 = Bool('down7')
up5 = Bool('up5')
# up7 = Bool('up7')
brakePedal = Bool('brakePedal')
# gasPedal = Bool('gasPedal')
# speed20 = Bool('speed20')
# prevDesiredSpeed = Bool('prevDesiredSpeed')
cruiseControlActivated = Bool('cruiseControlActivated')
targetSpeedIncrement = Bool('targetSpeedIncrement')
# targetSpeedIncrement10 = Bool('targetSpeedIncrement10')


# Set of variables that should not appear in action phase of the generated requirements
#RESTRICT_SET = [forward, backward, brakePedal, up5]

# Requirements that can be modified to solve an inconsistency
#REQ_MODIFY = [7]

REQ_SET = [
	# Req. 0
	# If the lever is pulled forward and brake is not pushed then,
	# the cruise control must be activated until the lever is pushed backward or brake is pushed.
	[ 
		And(forward, Not(brakePedal)), True, True, 0, 0, 1, 1, 
		cruiseControlActivated, cruiseControlActivated, Or(backward, brakePedal), 0, -1 
	],
	# Req. 1
	# If the lever is pushed backward then, the cruise control must not be active.
	[ 
		backward, True, True, 0, 0, 1, 1, True, True, Not(cruiseControlActivated), 0, 0 
	],
	# Req. 2
	# If the brake is pushed then, the cruise control must not be active.
	[ 
		brakePedal, True, True, 0, 0, 1, 1, True, True, Not(cruiseControlActivated), 0, 0 
	],
	# Req. 3
	# If the lever is pushed up and the cruise control is active then, 
	# the target speed of the cruise control must be increased.
	[ 
		And(up5, cruiseControlActivated), True, True, 0, 0, 1, 1, targetSpeedIncrement, True, True, 0, 0 
	],
	# Req. 4
	# If the cruise control is active and the lever is pushed up for 3 seconds then, 
	# the target speed of the cruise control must be increased until the lever leaves the position.
	[ 
		And(up5, cruiseControlActivated), up5, True, 2, 2, 0, 0, 
		targetSpeedIncrement, targetSpeedIncrement, Not(up5), 0, -1 
	],
	# Req. 5
	# The target speed is increased only when the cruise control is active.
	[ 
		targetSpeedIncrement, True, True, 0, 0, 0, 0, cruiseControlActivated, True, True, 0, 0 
	],
	
	# Req. 6
	# The lever position must not be forward and backward at the same time.
	[ 
		True, True, True, 0, 0, 0, 0, Or( Not(forward), Not(backward) ), True, True, 0, 0 
	]#,
	# To be generated for resolving an inconsistency
	# [ True, True, True, 0, 0, 0, 0, Or( And(Not(backward), Not(brakePedal)), Not(up5) ), True, True, 0, 0 ]
	# Req. 7
	# To be modified for resolving an inconsistency
	#[ True, True, True, 0, 0, 0, 0, Or( Not(backward), Not(up5) ), True, True, 0, 0 ]
	#[ False, True, True, 0, 0, 0, 0, Or( And(Not(backward), Not(brakePedal)), Not(up5) ), True, True, 0, 0 ]
]

COND_INIT = [ cruiseControlActivated == False, forward == False, backward == False, brakePedal == False ]











