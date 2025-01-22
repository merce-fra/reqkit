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

# Set of variables that should not appear in action phase of the generated requirements
#RESTRICT_SET = [downward5, downward7, neutral]

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
	]
	# To be generated for resolving an inconsistency
	# If the lever is moved downward 5 degrees less than 3 time units then,
	# after 5 time units, the lever position must be neutral.
	#[ 
	#	downward5, downward5, Not(downward5), 0, 3, 5, 5, 
	#	And( Not(downward5), Not(downward7) ), True, True, 0, 0 
	#]
]

COND_INIT = [ 
	downward5 == False, downward7 == False, neutral == True
]











