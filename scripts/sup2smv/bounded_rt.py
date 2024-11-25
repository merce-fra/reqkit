from z3 import *
from importlib import import_module
import sys
import os
import supreq
from supreq import State, init, trg, loc, act, err
# State, (init, trg, loc, act, err) = EnumSort( 'State', ['init', 'trg', 'loc', 'act', 'err'] )

def bounded_check(sup, req_set, alpha, verbose = False):
	s = Solver()
	s.add(sup.COND_INIT)

	for k in range(1, alpha):
		# S1: Add observer automata until step k-1
		for r in req_set:
			if r.step < k:
				r.model(r.step+1, k)
			s.add( r.formula[k-1] )

		# Escape the formula at S1
		s.push()

		# (!err & AX err)
		for r in req_set:
			s.add( r.st[k-1] != err )

		s.add( ForAll( 
			[ v[k] for v in sup.SYS_VAR ] 
			+ [ v[k] for r in req_set for v in [r.st, r.cnt, r.tt, r.ta] ], 
			Implies(
				And( [ f for r in req_set for f in r.formula[k] ] ),
				Or( [ r.st[k] == err for r in req_set ] )
			)
		))

		#"""Test"""
		#if verbose and k == 14:
		#	set_option(max_args=100000, max_lines=100000, max_depth=100000, max_visited=100000)
		#	with open('log.txt', mode='w') as f:
		#		f.write(str(s))

		if s.check() == sat:
			return s.model(), k-1
		else:
			if verbose:
				print("no inconsistency until step", k)
			# Restore the formula at S1
			s.pop()

	return [], -1

def print_trace(sup, sigma, step):
	for v in sup.SYS_VAR:
		for i in range(step+1):
			if sigma[ v[i] ] is not None:
				print(v[i], '=', sigma[ v[i] ])
			else:
				print(v[i], '= *')
		print()

if __name__ == "__main__":
	if len(sys.argv) < 2:
		print("Error: no input file")
		sys.exit()

	arg = sys.argv[1]

	if len(arg) > 3:
		if arg[len(arg)-3:len(arg)] == '.py':
			arg = arg[:len(arg)-3]

	if os.path.isfile(arg + '.py') == False:
		print("Error: file not found")
		sys.exit()

	mod = import_module(arg)
	sup = supreq.SUPInput(mod)

	sigma, step = bounded_check(sup, sup.REQ_SET, sup.ALPHA, True)
	if sigma == []:
		print("consistent")
	else:
		print("inconsistent")
		print_trace(sup, sigma, step)



