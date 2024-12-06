from z3 import *

# Specify a first upper bound of bounded partial consistency
ALPHA = 30

# Specify a second upper bound of bounded partial consistency
BETA = 10

# Specify a max length of positive traces for requirement improvement
MAX_PTRACE = 10

"""
Inputs:
- Lever positions: downward5, downward7, neutral
- Hazard light button: hazard

State variables:
- Light state: on, off
- Should the lights be flashing: flashing
- Is tip-blinking active: tip_blinking
- Has a tip-blinking request been given: tip_blink_request
"""

downward5 = Bool('downward5')
downward7 = Bool('downward7')
neutral = Bool('neutral')
# hazard = Bool('hazard')
light_on = Bool('light_on')
flashing = Bool('flashing')
# tip_blinking = Bool('tip_blinking')
# tip_blink_request = Bool('tip_blink_request')

REQ_SET = [
	# If the indicator light is on, it must keep on for 3 time units and then must turn off.
	[ 
		light_on, True, True, 0, 0, 0, 0, True, light_on, Not(light_on), 3, 3 
	],
	# If the indicator light is off for 3 time units and flashing request is given then,
	# the light must turn on.
	[ 
		Not(light_on), Not(light_on), And( Not(light_on), flashing ), 2, 2, 1, 1, True, True, light_on, 0, 0 
	],
	# If the lever is moved downward 7 degrees then, 
	# the indicator light must flash until the lever leaves its position.
	[ 
		downward7, True, True, 0, 0, 1, 1, flashing, flashing, Not(downward7), 0, -1 
	],
	# If the lever is moved downward 5 degrees less than 3 time units then,
	# the indicator light must flash for 6 time units.
	[ 
		downward5, downward5, Not(downward5), 0, 3, 0, 0, flashing, flashing, Not(flashing), 6, 6 
	],
	# If the lever is moved downward 5 degrees 3 time units or more then,
	# the indicator light must flash until the lever leaves its position.
	[ 
		downward5, downward5, True, 3, 3, 0, 0, flashing, flashing, Not(downward5), 0, -1 
	],
	# The lever position must be neutral, 5 degrees down, or 7 degrees down. 
	[ 
		True, True, True, 0, 0, 0, 0, 
		Or( 
			And( downward5, Not(downward7), Not(neutral) ), 
			And( Not(downward5), downward7, Not(neutral) ), 
			And( Not(downward5), Not(downward5), neutral ) 
		), 
		True, True, 0, 0 
	],
	# To be generated for resolving an inconsistency
	# If the lever is moved downward 5 degrees less than 3 time units then,
	# after 5 time units, the lever position must be neutral.
	[ 
		downward5, downward5, Not(downward5), 0, 3, 5, 5, 
		And( Not(downward5), Not(downward7) ), True, True, 0, 0 
	]
	# [ 
	# 	downward5, downward5, Not(downward5), 0, 3, 5, 5, 
	# 	Not(downward5), True, True, 0, 0 
	# ]
]

COND_INIT = [ 
	downward5 == False, downward7 == False, neutral == True
]
