from z3 import EnumSort, Bool, Int, Const, Solver, If, And, Or, Implies, Not, \
ForAll, Exists, simplify, sat, unsat, Optimize
from importlib import import_module
import sys
import os
import supreq
import bounded_rt
import qfree_zinf
from qfree_zinf import Literal, dc, pos, neg
from timeout_decorator import timeout, TimeoutError

VERBOSE = True
MAX_CLAUSE = 1
DUP_SUFFIX = '$'
# Literal, (dc, pos, neg) = EnumSort('Literal', ['dc', 'pos', 'neg'])
MAIN_TIMEOUT = 3600

def print_trace(sup, sigma, step):
	var_len = 0
	for v in sup.var_dic:
		if len(v) > var_len: var_len = len(v)
	sys.stdout.write( ''.ljust(var_len + 2) )

	for i in range(step+1):
		print(str(i % 10), end=' ')
	print()

	for v, j in list(sup.var_dic.items()):
		#print v + ':',
		sys.stdout.write(v.ljust(var_len) + ': ')
		v = sup.SYS_VAR[j]
		for i in range(step+1):
			sig = sigma[ v[i] ]
			if sig is not None:
				if str(sig.decl()) == 'True':
					print('T', end=' ')
				elif str(sig.decl()) == 'False':
					print('F', end=' ')
				else:
					print('\nerror at print_trace(): unknown trace')
			else:
				print('*', end=' ')
		print()

def print_req(sup, req):
	req_str = {}
	for cond in supreq.sup_cond:
		req_str[cond] = str( req[ supreq.sup_cond[cond] ] )
	for time in supreq.sup_time:
		req_str[time] = str( req[ supreq.sup_time[time] ] )

	print('(' + req_str['tse'] + ', ' + req_str['tc'] + ', ' + req_str['tee'] + ')['\
		+ req_str['tmin'] + ', ' + req_str['tmax'] + ']--['\
		+ req_str['lmin'] + ', ' + req_str['lmax'] + ']->('\
		+ req_str['ase'] + ', ' + req_str['ac'] + ', ' + req_str['aee'] + ')['\
		+ req_str['amin'] + ', ' + req_str['amax'] + ']')

def encode_req(sup, param_time, param_cond, max_cl):
	# SUP parameters TSE, TC, TEE, ASE, AC, AEE
	cond_formula = {}
	for cond in supreq.sup_cond:
		clause_set = []
		for j in range(max_cl):
			clause = []
			for i, v in enumerate(sup.SYS_VAR_RAW):
				clause = clause + [ If(
					param_cond[cond][i][j] == pos,
					v,
					If(
						param_cond[cond][i][j] == neg,
						Not(v),
						True
					)
				) ]
			#clause_set.append( simplify( And(clause) ) )
			clause_set.append( And(
				And(clause),
				If(
					And( [ param_cond[cond][i][j] == dc for i in range(len(sup.SYS_VAR_RAW)) ] ),
					False,
					True
				)
			))
		#cond_formula[cond] = simplify( Or(clause_set) )
		cond_formula[cond] = Or(
			Or(clause_set),
			If(
				And( [ 
					param_cond[cond][i][j] == dc 
					for i in range(len(sup.SYS_VAR_RAW)) 
					for j in range(max_cl) 
				] ),
				True,
				False
			)
		)

	# requirement encoding
	req_gen = []
	for i in sorted(supreq.sup_dic.values()):
		if supreq.sup_rev[i] in supreq.sup_cond:
			req_gen.append(cond_formula[ supreq.sup_rev[i] ])
		else:
			req_gen.append(param_time[ supreq.sup_rev[i] ])

	return req_gen

def decode_req(sup, model, param_time, param_cond, max_cl):
	cond_formula = {}
	for cond in supreq.sup_cond:
		clause_set = []
		for i in range(max_cl):
			clause = []
			for j in range(len(sup.SYS_VAR_RAW)):
				if str( model[ param_cond[cond][j][i] ] ) == 'pos':
					clause.append( sup.SYS_VAR_RAW[j] )
				elif str( model[ param_cond[cond][j][i] ] ) == 'neg':
					clause.append( Not( sup.SYS_VAR_RAW[j] ) )
				#elif str( model[ param_cond[cond][j][i] ] ) == 'bot':
				#	clause.append(False)
			#if clause == []:
			#	clause_set.append(True)
			#else:
			if clause != []:
				clause_set.append( simplify( And(clause) ) )
		#cond_formula[cond] = simplify( Or(clause_set) )
		if clause_set != []:
			cond_formula[cond] = simplify( Or(clause_set) )
		else:
			cond_formula[cond] = True

	req_generated = []
	for i in range(len(supreq.sup_dic)):
		if supreq.sup_rev[i] in supreq.sup_cond:
			req_generated.append( cond_formula[supreq.sup_rev[i]] )
		elif model[ param_time[supreq.sup_rev[i]] ] is not None:
			req_generated.append( model[ param_time[supreq.sup_rev[i]] ].as_long() )
		else:
			req_generated.append(0)

	return req_generated

def is_witness(sup, sigma, step):
	sup.sys_var_decl(step)

	s = Solver()
	s.add(sup.COND_INIT)

	for v in sup.SYS_VAR:
		for i in range(step+1):
			if sigma[ v[i] ] is not None:
				s.add( v[i] == sigma[ v[i] ] )

	# Encode observer automata
	for r in sup.REQ_SET:
		r.model(0, step+1)
		for i in range(step+2):
			s.add( r.formula[i] )
		# There exists one step extension without error
		s.add( Not( r.st[step+1] == supreq.err ) )

	return s.check()

# Search a trace triggering the requirement 'req' other than traces in 't_list'
def vacuity_trace(sup, req_set, req, t_list, alpha, beta):
	s = Solver()
	s.add(sup.COND_INIT)

	# Check if 'req' can be triggered by step k in (0, alpha-1)
	for k in range(alpha):
		# S1: Add observer automata for step k
		for r in req_set:
			if r.step < k:
				r.model(r.step + 1, k)
			s.add( r.formula[k] )

		# Escape the formula at S1
		s.push()

		# 'req' is triggered at step k
		s.add( req.tt[k] == k )

		# exclude given traces
		for tr, step in t_list:
			#if step <= k:
			val = []
			for i in range(step+1):
				sup.sys_var_decl(i)
				for v in sup.SYS_VAR:
					if tr[v[i]] is not None:
						val.append( v[i] == tr[v[i]] )
			s.add( Not( And(val) ) )

		if s.check() == unsat:
			# Restore the formula at S1
			s.pop()
			continue

		# Check if there exists a lasso-shaped trace less than (k + beta)
		for l in range(1, beta+1):
			# S2: Add observer automata for step (k + l)
			for r in req_set:
				if r.step < k + l:
					r.model(r.step + 1, k + l)
				s.add( r.formula[k+l] )

			# Escape the formula at S2
			s.push()

			for r in req_set:
				# Requirements are not violated until step (k + l)
				s.add( r.st[k+l] != supreq.err )
				# sigma[k...k+l] can be iterated infinitely many times without error
				s.add( r.st[k] == r.st[k+l], r.cnt[k] == r.cnt[k+l] )

			if s.check() == sat:
				sigma = s.model()
				return sigma, k

			# Restore the formula at S2
			s.pop()
		# Restore the formula at S1
		s.pop()
		#return [], -1
	# There is no such traces
	return [], -1

def vacuity_check(sup, req_check, req_generated, gen_label):
	sigma, step = vacuity_trace(sup, sup.REQ_SET + [req_generated], req_check, [], sup.MAX_PTRACE, sup.BETA)
	rstr = []	
	for r in sup.REQ_SET:
		rstr.append('Req.' + str(r.id))
	rstr.append('NewReq.' + str(gen_label))

	if sigma != []:
		# 'req_set + [req_generated]' is not vacuous for 'req_check'
		print('{' + ', '.join(rstr) + '} are not vacuous for Req.' + str(req_check.id))
		return [], -1

	sigma, step = vacuity_trace(sup, sup.REQ_SET, req_check, [], sup.MAX_PTRACE, sup.BETA)
	if sigma != []:
		# 'req_set + [req_generated]' is vacuous for 'req_check'
		# Return a positive trace triggering 'req_check' without violating 'req_set' as witness
		print('{' + ', '.join(rstr) + '} are vacuous for Req.' + str(req_check.id))

		if VERBOSE: 
			print('\nTrace found:')
			print_trace(sup, sigma, step)
			print()

		return sigma, step
	else:
		# 'req_set' itself is vacuous for 'req_check'
		print('{' + ', '.join(rstr[:-1]) + '} are originally vacuous for Req.' + str(req_check.id))
		return [], -1

def refine_vacuity(sup, solver, idx, req_check, req_gen_orig, param_time, param_cond, gen_label, max_cl, cost_exp):
	block_list = []
	copy = sup.duplicate( DUP_SUFFIX + str(idx) )
	req_gen = encode_req(copy, param_time, param_cond, max_cl)
	req_gen = supreq.Requirement(copy, req_gen, idx)
	
	sigma, step = vacuity_check(sup, req_check, req_gen_orig, gen_label)
	
	if sigma == []:
		sigma, step = vacuity_trace(sup, sup.REQ_SET + [req_gen_orig], req_check, [], sup.MAX_PTRACE, sup.BETA)
		
		# req_check is originally vacuous
		if sigma == []:
			return req_gen_orig, solver, idx, gen_label
		
		trace = []
		copy.sys_var_decl(step)
		for i, v in enumerate(sup.SYS_VAR):
			for j in range(step+1):
				if sigma[ v[j] ] is not None:
					trace.append( copy.SYS_VAR[i][j] == sigma[ v[j] ] )
		
		req_gen.model(0, step)
		solver.add( And(
			[ f for fset in req_gen.formula for f in fset ] + trace + copy.COND_INIT + [
				req_gen.st[step] != supreq.err
			]
		) )
		return req_gen_orig, solver, idx + 1, gen_label
	else:
		print("Try to refine...\n")

	cnt = 1
	while True:
		current_ptrace = (sigma, step)
	
		print("Vacuity resolving, Iteration: " + str(cnt))
		req_gen.model(0, step)

		trace = []
		var_dc = []
		copy.sys_var_decl(step)
		for i, v in enumerate(sup.SYS_VAR):
			for j in range(step+1):
				if sigma[ v[j] ] is not None:
					trace.append( copy.SYS_VAR[i][j] == sigma[ v[j] ] )
				else:
					var_dc.append(copy.SYS_VAR[i][j])

		# Escape the formula given as the argument
		solver.push()

		# Generated requirement should accept the trace
		solver.add( And(
			[ f for fset in req_gen.formula for f in fset ] + trace + copy.COND_INIT + [
				req_gen.st[step] != supreq.err
			]
		) )

		# maxsat
		cost = Int('cost')
		opt = Optimize()
		opt.add(solver.assertions())
		opt.add(cost == cost_exp)
		lb = opt.minimize(cost)

		if opt.check() == sat:
			gen_label += 1
			model = opt.model()
			print("cost = " + str(opt.lower(lb)))
			print("NewReq." + str(gen_label) + " generated:", end=' ')
			req_gen = decode_req(sup, model, param_time, param_cond, max_cl)
			print_req(sup, req_gen)
			req_gen = supreq.Requirement(sup, req_gen, req_gen_orig.id)

			sigma, step = bounded_rt.bounded_check( sup, sup.REQ_SET + [req_gen], sup.ALPHA )
			if sigma == []: 
				print('Refinement on vacuity for Req.' + str(req_check.id) + ' succeeded\n')
				return req_gen, solver, idx + 1, gen_label
			else:
				print("Generated requirement cannot resolve inconsistency\n")

				if VERBOSE: 
					print('Trace found:')
					print_trace(sup, sigma, step)
					print()

				req_gen, solver, idx, gen_label = \
				fix_rt(sup, solver, idx + 1, sigma, step, param_time, param_cond, gen_label+1, max_cl, cost_exp)
				
				if req_gen != []:
					req_gen = supreq.Requirement(sup, req_gen, req_gen_orig.id)
					print('Refinement on vacuity for Req.' + str(req_check.id) + ' succeeded\n')
					return req_gen, solver, idx, gen_label
				else:
					copy = sup.duplicate( DUP_SUFFIX + str(idx) )
					req_gen = encode_req(copy, param_time, param_cond, max_cl)
					req_gen = supreq.Requirement(copy, req_gen, idx)

		print("Current positive trace is not compatible with inconsistency fixing\n")
		block_list.append( current_ptrace )
		
		# Restore the formula given as the argument
		solver.pop()

		sigma, step = vacuity_trace(sup, sup.REQ_SET, req_check, block_list, sup.MAX_PTRACE, sup.BETA)
		if sigma == []:
			print("Vacuity for Req." + str(req_check.id) + " cannot be resolved\n")
			return req_gen_orig, solver, idx, gen_label
		print("New positive trace was found\n")
		
		if VERBOSE: 
			print_trace(sup, sigma, step)
			print()

		cnt += 1

def fix_rt(sup, solver, idx, sigma, step, param_time, param_cond, gen_label, max_cl, cost_exp):
	MAX_ITER = 1000
	cnt = 1

	# maxsat
	cost = Int('cost')

	while True:
		if cnt > MAX_ITER:
			print("Iteration reached to the upperbound")
			return [], None, None, gen_label

		"""copy = sup.duplicate( DUP_SUFFIX + str(idx) )
		req_gen = encode_req(copy, param_time, param_cond, max_cl)
		req_gen = supreq.Requirement(copy, req_gen, idx)

		trace = []
		var_dc = []
		copy.sys_var_decl(step)
		for i, v in enumerate(sup.SYS_VAR):
			for j in range(step+1):
				if sigma[ v[j] ] is not None:
					trace.append( copy.SYS_VAR[i][j] == sigma[ v[j] ] )
				else:
					var_dc.append(copy.SYS_VAR[i][j])"""

		# C1 <- sigma: generated requirements cannot remove the conflict caused by 'sigma'
		if is_witness(sup, sigma, step) == unsat:
			idx += 1
			if cnt > 1: print("Generated requirement cannot resolve inconsistency\n")

			if cnt > 1 and VERBOSE: 
				print('Trace found:')
				print_trace(sup, sigma, step)
				print()

			"""req_gen.model(0, step)

			# Generated requirement should be violated by the trace
			if var_dc == []:
				solver.add( And(
					trace + [ f for fset in req_gen.formula for f in fset ] + copy.COND_INIT + [
						#req_gen.st[0] != supreq.err, 
						req_gen.st[step] == supreq.err
					]
				) )
			else:
				solver.add( ForAll( var_dc, And(
					trace + [ f for fset in req_gen.formula for f in fset ] + copy.COND_INIT + [
						#req_gen.st[0] != supreq.err, 
						req_gen.st[step] == supreq.err
					]
				) ) )"""

			st = qfree_zinf.qfree_encode(sup, sigma, step, param_time, param_cond)
			solver.add( Not( st['err'][0] ) )
			solver.add( And( 
				st['err'][step], Not( st['init'][step] ), Not( st['trg'][step] ), Not( st['loc'][step] ), Not( st['act'][step] ) 
			) )

		# C2 <- sigma: generated requirements introduced the new conflict caused by 'sigma'
		else:
			if cnt > 1: print("Generated requirement introduces new inconsistency\n")

			if cnt > 1 and VERBOSE: 
				print('Trace found:')
				print_trace(sup, sigma, step)
				print()

			copy = sup.duplicate( DUP_SUFFIX + str(idx) )
			copy, idx = sup.duplicate_req( copy, DUP_SUFFIX + str(idx), idx + 1 )
			solver = qfree_zinf.qfree_test(sup, sigma, step, param_time, param_cond, solver, copy)

			"""copy, idx = sup.duplicate_req( copy, DUP_SUFFIX + str(idx), idx + 1 )
			req_gen.model(0, step+1)
			for r in copy.REQ_SET:
				r.model(0, step+1)

			# Generated requirement should be violated by the trace
			# Otherwise, the trace could be extended one step without violating all the requirements
			if var_dc == []:
				solver.add( And(
					And(
						[ f for formula in req_gen.formula for f in formula ] 
						+ [ f for r in copy.REQ_SET for formula in r.formula for f in formula ] 
						#+ [ req_gen.st[0] != supreq.err ]
						+ trace + copy.COND_INIT 
					), 
					Or(
						req_gen.st[step] == supreq.err,
						And( [ r.st[step+1] != supreq.err for r in copy.REQ_SET + [req_gen] ] )
					)
				))
			else:
				solver.add( ForAll( var_dc, And(
					And(
						[ f for formula in req_gen.formula for f in formula ] 
						+ [ f for r in copy.REQ_SET for formula in r.formula for f in formula ] 
						#+ [ req_gen.st[0] != supreq.err ]
						+ trace + copy.COND_INIT 
					), 
					Or(
						req_gen.st[step] == supreq.err,
						And( [ r.st[step+1] != supreq.err for r in copy.REQ_SET + [req_gen] ] )
					)
				)))"""

		print("Inconsistency resolving, Iteration: " + str(cnt))

		# maxsat
		opt = Optimize()
		opt.add(solver.assertions())
		opt.add(cost == cost_exp)
		lb = opt.minimize(cost)
		
		if opt.check() == unsat:
			print("Generation failed: no solution for maxsat")
			return [], solver, idx, gen_label

		model = opt.model()
		print("cost = " + str(opt.lower(lb)))

		print("NewReq." + str(gen_label) + " generated:", end=' ')
		req_gen = decode_req(sup, model, param_time, param_cond, max_cl)
		print_req(sup, req_gen)

		sigma, step = bounded_rt.bounded_check(
			sup, sup.REQ_SET + [ supreq.Requirement( sup, req_gen, len(sup.REQ_SET) ) ], sup.ALPHA
		)

		if sigma == []: 
			rstr = []
			for r in sup.REQ_SET:
				rstr.append('Req.' + str(r.id))
			rstr.append('NewReq.' + str(gen_label))
			print('Inconsistency resolving succeeded: {' + ', '.join(rstr) + '} are consistent.\n')
			return req_gen, solver, idx, gen_label

		cnt += 1
		gen_label += 1

def sup_parse(sup, symbol, cond, param_modify):
	def parse(symbol):
		clause = [dc] * len(sup.var_dic)
		if symbol.num_args() == 0:
			idx = sup.var_dic[ symbol.decl().name() ]
			clause[idx] = pos
		elif symbol.decl().name() == 'not':
			idx = sup.var_dic[ symbol.arg(0).decl().name() ]
			clause[idx] = neg
		elif symbol.decl().name() == 'and':
			for child in symbol.children():
				if child.num_args() == 0:
					idx = sup.var_dic[ child.decl().name() ]
					clause[idx] = pos
				elif child.decl().name() == 'not':
					idx = sup.var_dic[ child.arg(0).decl().name() ]
					clause[idx] = neg
				else:
					print("error at sup_parse(): given formula is not CNF")
					print(symbol)
					print(child)
					sys.exit()
		else:
			print("error at sup_parse(): given formula is not CNF")
			print(symbol)
			sys.exit()
		return clause

	if type(symbol) is bool:
	#	if symbol:
	#		return
	#	else:
	#		param_modify[cond][0][0] = bot
	#		return
		return
	elif symbol.decl().name() == 'or':
		for i in range( symbol.num_args() ):
			param_modify[cond][i] = parse( symbol.arg(i) )
	else:
		param_modify[cond][0] = parse( symbol )

@timeout(MAIN_TIMEOUT)
def main():
	# file input
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

	print("Input requirements:")
	for r in sup.REQ_SET:
		print("    Req." + str(r.id) + ":", end=' ')
		print_req(sup, r.req)
	print()

	# consistency check
	sigma, step = bounded_rt.bounded_check(sup, sup.REQ_SET, sup.ALPHA)
	if sigma == []:
		print("Given requirements are consistent: no need to fix")
		sys.exit()

	print("Given requirements are inconsistent.")

	if VERBOSE: 
		print('\nTrace found:')
		print_trace(sup, sigma, step)
		print()

	print("Try to fix...\n")
	
	# check if there exists modifiable requirements
	flg_modify = False
	if "REQ_MODIFY" in mod.__dict__:
		if len(mod.REQ_MODIFY) > 0:
			flg_modify = True

	# set clause number of generating/modifying requirement
	max_cl = MAX_CLAUSE
	if flg_modify:
		# input modifiable requirements (currently only one requirement can be modified)
		req_modify = sup.REQ_SET[ mod.REQ_MODIFY[0] ]
		
		print("Modifiable requirement:")
		print("    Req." + str(req_modify.id) + ":", end=' ')
		print_req(sup, req_modify.req)

		# count clause number of modifiable requirements
		for cond in list(supreq.sup_cond.values()):
			p = req_modify.req[cond]
			if type(p) is not bool:	
				if p.decl().name() == 'or':
					if max_cl < p.num_args():
						max_cl = p.num_args()
	else:
		max_cl = MAX_CLAUSE

	# SUP parameters Tmin, Tmax, Lmin, Lmax, Amin, Amax
	param_time = { t : Int(t) for t in supreq.sup_time }

	# SUP parameters TSE, TC, TEE, ASE, AC, AEE
	param_cond = {}
	for cond in supreq.sup_cond:
		lt = [None] * len(sup.var_dic)
		for v, i in list(sup.var_dic.items()):
			lt[i] = [ 
				Const(cond + '_' + str(j) + '_' + v, Literal) 
			for j in range(max_cl) ]
		param_cond[cond] = lt

	# define distance function for modifiable requirement
	cost_exp = 0
	if flg_modify:
		param_modify = {}
		
		for cond in supreq.sup_cond:
			#lt = [ [dc] * len(sup.var_dic) ] + [ [bot] * len(sup.var_dic) ] * (max_cl - 1)
			lt = [ [dc] * len(sup.var_dic) ] * (max_cl)
			param_modify[cond] = lt
		for cond, i in list(supreq.sup_cond.items()):
			sup_parse(sup, req_modify.req[i], cond, param_modify)
		for time, i in list(supreq.sup_time.items()):
			param_modify[time] = req_modify.req[i]
		for cond in supreq.sup_cond:
			for i in list(sup.var_dic.values()):
				for j in range(max_cl):
					cost_exp = cost_exp + If(
						param_cond[cond][i][j] == param_modify[cond][j][i],
						0,
						1
					)
		for time in supreq.sup_time:
			cost_exp = cost_exp + If(
				param_time[time] == param_modify[time],
				0,
				1
			)
	else:
		for cond in supreq.sup_cond:
			for i in list(sup.var_dic.values()):
				#cost_exp = cost_exp + If(
				#	param_cond[cond][i][0] == dc,
				#	0,
				#	1
				#)
				#for j in range(1, max_cl):
				for j in range(max_cl):
					cost_exp = cost_exp + If(
						#param_cond[cond][i][j] == bot,
						param_cond[cond][i][j] == dc,
						0,
						1
					)
		for time in supreq.sup_time:
			cost_exp = cost_exp + If(
				param_time[time] == 0,
				0,
				1
			)

	# configure solver instance
	s = Solver()
	s.add(
		#0 == param_time['tmin'],
		#0 == param_time['lmin'],
		#0 == param_time['amin'],
		#Or( 0 == param_time['tmax'], -1 == param_time['tmax'] ),
		#Or( 0 == param_time['lmax'], -1 == param_time['lmax'] ),
		#Or( 0 == param_time['amax'], -1 == param_time['amax'] ),
		0 == param_time['tmin'],
		0 == param_time['lmin'],
		0 == param_time['amin'],
		0 == param_time['tmax'],
		0 == param_time['lmax'],
		0 == param_time['amax'],
		# restrict empty requirements
		Not(And( [ 
			param_cond[cond][i][j] == dc 
			for cond in ['ase', 'ac', 'aee']
			for i in list(sup.var_dic.values()) 
			for j in range(max_cl) 
		] ))
	)
	
	# restrict user operations from appearing in action phase
	if "RESTRICT_SET" in mod.__dict__:
		print("The following variables are restricted from appearing in the action phase:")
		print(mod.RESTRICT_SET)
		print()
		
		for restrict in mod.RESTRICT_SET:
			for param in ['ase', 'ac', 'aee']:
				for j in range(max_cl):
					s.add(
						param_cond[ param ][ sup.var_dic[restrict.decl().name()] ][j] == dc
					)
	
	# remove modifiable requirement from original set
	if flg_modify:
		req_set = sup.REQ_SET
		req_set.pop(mod.REQ_MODIFY[0])
		sup.REQ_SET = req_set

	# fix rt-consistency
	req_gen, s, idx, gen_label = fix_rt(sup, s, 0, sigma, step, param_time, param_cond, 0, max_cl, cost_exp)
	
	# Vacuity resolving
	if req_gen != []:
		req_gen = supreq.Requirement( sup, req_gen, len(sup.REQ_SET) )
		for r in sup.REQ_SET:
			req_gen, s, idx, gen_label = \
			refine_vacuity(sup, s, idx, r, req_gen, param_time, param_cond, gen_label, max_cl, cost_exp)
	
	print("\nGenration/modification result:")
	for r in sup.REQ_SET:
		print("    Req." + str(r.id) + ":", end=' ')
		print_req(sup, r.req)
	print("    NewReq." + str(gen_label) + ":", end=' ')
	print_req(sup, req_gen.req)

if __name__ == "__main__":
	try:
		main()
	except TimeoutError as e:
		print(e)
		sys.exit()










