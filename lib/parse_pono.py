# Copyright 2025 Mitsubishi Electric R&D Centre Europe
# Author: Ocan Sankur
# Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
# 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
# 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

import sys
import re

state_count = 0
sup_loc = [dict(), dict()]
sup_err = dict()
sup_clock = dict()
vacuity = dict()
delay = None
inputs = dict()
ltl_states = dict()

def display_state(final = False):
  global sup_loc, sup_err, vacuity, sup_clock, delay, inputs
  ids = sorted(sup_loc[0].keys())
  if len(ids) == 0:
    return
  print("\tSUP Automata States:")
  for id in ids:
    if sup_err[id]:
      print(f"\tState({id}): ERR, c={sup_clock[id]}")
    elif (not sup_loc[0][id]) and not sup_loc[1][id]:
      print(f"\tState({id}): IDLE, c={sup_clock[id]}")
    elif (not sup_loc[0][id]) and sup_loc[1][id]:
      print(f"\tState({id}): TRIG, c={sup_clock[id]}")
    elif (sup_loc[0][id]) and not sup_loc[0][id]:
      print(f"\tState({id}): DELAY, c={sup_clock[id]}")
    else:
      print(f"\tState({id}): ACTION, c={sup_clock[id]}")
    if id in vacuity:
      print(f"\tNon-Vacuity witness: {not vacuity[id]}")
  print(f"")
  if len(ltl_states)>0:
    print("\tLTL Observer:")
    for (id, v) in ltl_states.items():
      print(f"\t{id} = {v}")
    print("")
  if not final:
    print("\tINPUT:")
    for (id, v) in inputs.items():
      print(f"\t{id} = {v}")
    print(f"\tDELAY = {delay}")
    print(f"")

def reset_state():
  global sup_loc, sup_err, vacuity, sup_clock, delay, inputs
  sup_loc = [dict(), dict()]
  sup_err = dict()
  vacuity = dict()
  sup_clock = dict()
  delay = None
  inputs = dict()

lines = []
error = False
for line in sys.stdin:
  lines.append(line)
  if line.strip() == "error":
    error = True

if not error:
  for line in lines:
    mTime = re.search('AT TIME (.*)', line)
    mDelta = re.search('\w*_DELTA_ : (.*)', line)
    mLoc = re.search('\w*state_(.*)_0_loc(.*) : (.*)', line)
    mErr = re.search('\w*state_(.*)_0_err : (.*)', line)
    mVac = re.search('\w*vacuity_(.*)_0 : (.*)', line)
    mClock = re.search('\w*c_(.*)_0 : (.*)', line)
    mInput = re.search('\w*(.*) : (.*)', line)

    if mTime:
      display_state()
      reset_state()
      print(line,end="")
    elif mDelta:
      delay = mDelta.group(1)
      # print(f"Delay: {mDelta.group(1)}")
    elif mLoc:
      id = mLoc.group(1).strip()
      bit = int(mLoc.group(2))
      v = mLoc.group(3).strip()
      sup_loc[bit][id] = (v == "true")
      # print(f"-- state {id} loc{bit} = {v}")
    elif mErr:
      id = mErr.group(1).strip()
      v = mErr.group(2).strip()
      sup_err[id] = (v == "true")
      # print(f"err {id} = {v}")
    elif mVac:
      id = mVac.group(1).strip()
      v = mVac.group(2).strip()
      vacuity[id] = (v == "true")
      # print(f"vac {id} = {v}")
    elif mClock:
      id = mClock.group(1).strip()
      v = mClock.group(2).strip()
      sup_clock[id] = v
      # print(f"clock {id} = {v}")
    elif mInput:
      id = mInput.group(1).strip()
      v = mInput.group(2).strip()
      if "ltl_" in id:
        ltl_states[id] = v
      else:
        inputs[id] = v
      # print(f"input {id} = {v}")
    
  display_state(True)
else:
  for line in lines:
    print(line, end="")