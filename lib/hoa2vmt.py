import re
import sys
import math
import logging
"""
  Translate HOA Buchi automaton given as input to the VMT format, interpreting it as a finite automaton.

  For example:

    States: 5
    Start: 2
    AP: 3 "b" "a" "c"
    acc-name: Buchi
    Acceptance: 1 Inf(0)
    properties: trans-labels explicit-labels state-acc complete
    properties: deterministic
    --BODY--
    State: 0 {0}
    [!2] 0
    [2] 1
    State: 1 {0}
    [t] 1
    State: 2
    [0&!2] 0
    [0&2] 1
    [!0&1&!2] 2
    [!0&1&2] 3
    [!0&!1] 4
    State: 3
    [0] 1
    [!0&1] 3
    [!0&!1] 4
    State: 4
    [t] 4

"""

logger = logging.Logger("Hoa2VMT")

class Hoa2VMT:
  nb_states : int
  init_state : int
  # ap[i] is the name of the i-th AP
  ap : list[str]
  # original input string describing the HOA
  input_str : str
  # list of accepting state ids
  accepting_states : list[int]
  transitions : list[int, str, int]

  # name prefix for bool variables encoding states
  bit_name_prefix : str
  # total nb of bits required to encode states
  nb_bits : int
  def __init__(self, input_str : str):
    self.input_str = input_str
    self.nb_states = None
    self.init_state = None
    self.acceptance_label = None
    self.ap = []
    self.current_state = None # currently parsed state
    self.accepting_states = []
    self.transitions = []
    self.bit_name_prefix = "ltl_aut_bit_"
    self.nb_bits = None
    self.parse()
    self.dump_vmt()

  def _dump_header(self):
    """ Dump Boolean encoding for state variables, and functions for states """
    # for ap in self.ap:
    #   print(f"(declare-fun {ap} () Bool)")
    for i in range(self.nb_bits):
      print(f"(declare-fun {self._bitname(i)} () Bool)")
      print(f"(declare-fun {self._bitname(i)}_next () Bool)")
      print(f"(define-fun ltl_next_state_{i} () Bool (! {self._bitname(i)} :next {self._bitname(i)}_next))")
    for state in range(self.nb_states):
      print(f"; State {state}: {self._encode_state(state)}")
      print(f"(define-fun is_ltl_state_{state} () Bool {self._encode_state(state)})")
      print(f"(define-fun set_ltl_state_{state} () Bool {self._encode_state(state, next=True)})")
    
    print(f"; Init state {self.init_state}: {self._encode_state(self.init_state)}")
    print(f"(define-fun ltl_init () Bool (! {self._encode_state(self.init_state)} :init true))")

  def _bitname(self, no : int, next=False):
    if next:
      return f"{self.bit_name_prefix}{no}_next"
    else:
      return f"{self.bit_name_prefix}{no}"
  
  def _encode_state(self, state : int, next=False):
    """ Return VMT encoding of given state id """
    enc = []
    for b in range(self.nb_bits):
      if (state >> b) % 2 == 1:
        enc.append(self._bitname(b, next))
      else:
        enc.append(f"(not {self._bitname(b, next)})")
    if next:
      enc = list(map(lambda x: f"{x}", enc))
    if len(enc) == 1:
      return enc[0]
    else:
      return "(and " + " ".join(enc) + ")"
    
  def _encode_guard(self, guard : str):
    """ Parse guard of the form !0 && 1 && !2 into VMT """
    def map_conjunct(c):
      c = c.strip()
      if c[0] == "!":
        return f"(not {self.ap[int(c[1:])]})"
      else:
        return self.ap[int(c)]
    if guard == "t":
      return "true"
    elif guard == "f":
      return "false"
    elif "|" in guard:
      disjuncts = guard.split("|")
      return f"(or {' '.join([self._encode_guard(disj) for disj in disjuncts] )})"
    else:
      conjuncts = list(map(map_conjunct, guard.split("&")))
      return " ".join(conjuncts)      

  def dump_vmt(self):
    self._dump_header()
    print(f"(define-fun .ltl_trans () Bool (!  (or")
    for t in self.transitions:
      print(f"\t(and is_ltl_state_{t[0]} {self._encode_guard(t[1])} set_ltl_state_{t[2]})")
    print("        ) :trans true))")
    if len(self.accepting_states) > 1:
      ltl_acc_states = "(or " + " ".join(map(lambda s: f"{self._encode_state(s)}", self.accepting_states)) + ")"
    else:
      ltl_acc_states = self._encode_state(self.accepting_states[0])
    print(f"; non-error states must imply ltl accepting states")
    print(f"(define-fun .ltl-prop () Bool (! (or (not .all_sup_status) {ltl_acc_states}) :invar-property 0))")

  def parse(self):
    for line in self.input_str:
      if "HOA:" in line or "properties:" in line or "acc-name:" in line or "--BODY--" in line or "--END--" in line or line.strip() == "":
        continue
      m = re.match("\W*States\W*:\W*([0-9]+).*", line)
      if m:
        self.nb_states = int(m.group(1))
        self.nb_bits = int(math.ceil(math.log(self.nb_states, 2)))
        logger.debug(f"-> nb states: {self.nb_states}")
        continue
      m = re.match("\W*Start\W*:\W*([0-9]+).*", line)
      if m:
        self.init_state = int(m.group(1))
        logger.debug(f"-> init state: {self.init_state}")
        continue

      m = re.match("\W*AP\W*:\W*([0-9]+)(.*)", line)
      if m:
        self.ap = m.group(2).strip().split(' ')
        # remove ""
        self.ap = list(map(lambda x: x[1:-1], self.ap))
        logger.debug(f"-> AP: {self.ap}")
        continue

      m = re.match("\W*Acceptance\W*:\W*1\W*Inf\(([0-9]*)\)", line)
      if m:
        self.acceptance_label = m.group(1)
        logger.debug(f"-> Acceptance: {self.acceptance_label}")
        continue

      mAcc = re.match("\W*State\W*:\W*([0-9]*) {[0-9]*}", line)
      mNonAcc = re.match("\W*State\W*:\W*([0-9]*).*", line)
      if mAcc:
        self.current_state = int(mAcc.group(1))
        self.accepting_states.append(self.current_state)
        logger.debug(f"-> Current state: {self.current_state} (accepting)")
        continue
      elif mNonAcc:
        self.current_state = int(mNonAcc.group(1))
        logger.debug(f"-> Current state: {self.current_state}")
        continue

      m = re.match("\W*\[(.*)\]\W*([0-9]*).*", line)
      if m:
        target_state = m.group(2)
        guard = m.group(1)
        logger.debug(f"-> Transition {self.current_state} to {target_state} if {guard}")
        self.transitions.append((self.current_state, guard, target_state))
        continue
      logger.debug(f"Ignored: {line}")

logger.setLevel(logging.DEBUG)
translator = Hoa2VMT(sys.stdin.read().split('\n'))
