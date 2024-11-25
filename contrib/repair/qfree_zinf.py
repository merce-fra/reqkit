from z3 import *
from importlib import import_module
import sys
import os
import supreq
#import bounded_rt
#import reqgen
import itertools

VERBOSE = True
MAX_CLAUSE = 1
SUP_STATE = ['init', 'trg', 'loc', 'act', 'err']
Literal, (dc, pos, neg) = EnumSort('Literal', ['dc', 'pos', 'neg'])

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
					print('\nerror at print_trace_b(): unknown trace')
			else:
				print('*', end=' ')
		print()

def print_state(sup, sigma, step):
	for r in sup.REQ_SET:
		if sigma[ r.st[step] ] is not None:
				print(r.st[step], '=', sigma[ r.st[step] ])
		else:
			print(r.st[step], '= *')
		if sigma[ r.cnt[step] ] is not None:
				print(r.cnt[step], '=', sigma[ r.cnt[step] ])
		else:
			print(r.cnt[step], '= *')
		print()

def print_req_gen(sup, model, param_time, param_cond):
	req_str = {}
	for cond in supreq.sup_cond:
		clause = []
		for i in range(len(sup.SYS_VAR)):
			name = sup.SYS_VAR[i][0].decl().name()
			base = name[:len(name)-3]
			if str( model[ param_cond[cond][i][0] ] ) == 'pos':
				clause.append(base)
			elif str( model[ param_cond[cond][i][0] ] ) == 'neg':
				clause.append('!' + base)
		if clause == []:
			req_str[cond] = 'True'
		else:
			req_str[cond] = ' && '.join(clause)

	for time in supreq.sup_time:
		if model[ param_time[time] ] is not None:
			req_str[time] = str( model[ param_time[time] ] )
		else:
			req_str[time] = str(0)

	print('(' + req_str['tse'] + ', ' + req_str['tc'] + ', ' + req_str['tee'] + ')['\
		+ req_str['tmin'] + ', ' + req_str['tmax'] + ']--['\
		+ req_str['lmin'] + ', ' + req_str['lmax'] + ']->('\
		+ req_str['ase'] + ', ' + req_str['ac'] + ', ' + req_str['aee'] + ')['\
		+ req_str['amin'] + ', ' + req_str['amax'] + ']')

def qfree_untimed(sup, sigma, step, param_time, param_cond):
	# st[q][i] <=> (s_i^q & c_i^q[0]) <=> (q, 0) is reachable at step i
	st = { s : { i : False for i in range(step+1) } for s in SUP_STATE }
	#print st
	
	def cond_pos(cond, step):
		clause = []
		for i in list(sup.var_dic.values()):
			v = sup.SYS_VAR[i]
			if sigma[ v[step] ] is not None:
				if sigma[ v[step] ] == True:
					clause.append( param_cond[cond][i][0] != neg )
				elif sigma[ v[step] ] == False:
					clause.append( param_cond[cond][i][0] != pos )
		return And(clause)
		
	def cond_neg(cond, step):
		clause = []
		for i in list(sup.var_dic.values()):
			v = sup.SYS_VAR[i]
			if sigma[ v[step] ] is not None:
				if sigma[ v[step] ] == True:
					clause.append( param_cond[cond][i][0] == neg )
				elif sigma[ v[step] ] == False:
					clause.append( param_cond[cond][i][0] == pos )
		return Or(clause)
	
	def cond_pos_neg(cpos_list, cneg, step):
		clause = []
		for i in list(sup.var_dic.values()):
			v = sup.SYS_VAR[i]
			if sigma[ v[step] ] is None:
				clause.append( And(
					[ 
						param_cond[ cpos_list[j] ][i][0] != param_cond[cneg][i][0] 
						for j in range( len(cpos_list) )
					] + [ param_cond[cneg][i][0] != dc ]
				) )
			elif sigma[ v[step] ] == True:
				clause.append( param_cond[cneg][i][0] == neg )
			elif sigma[ v[step] ] == False:
				clause.append( param_cond[cneg][i][0] == pos )
		return Or(clause)
	
	def cond_pos_pos(cond_list, step):
		clause = []

		if len(cond_list) > 2:
			for (i, j) in itertools.combinations( list(range(len(cond_list))), 2 ):
				clause.append( cond_pos_pos( [ cond_list[i], cond_list[j] ], step ) )
			return And(clause)
		
		if len(cond_list) == 2:
			for i in list(sup.var_dic.values()):
				v = sup.SYS_VAR[i]
				if sigma[ v[step] ] is None:
					clause.append( Or(
						param_cond[ cond_list[0] ][i][0] == param_cond[ cond_list[1] ][i][0],
						param_cond[ cond_list[0] ][i][0] == dc,
						param_cond[ cond_list[1] ][i][0] == dc
					) )
			return And(clause)
	
	for i in range(step+1):
		# !tse | !tee | (ase & aee)
		st['init'][i] = Or(
			cond_neg('tse', i), cond_neg('tee', i),
			And( cond_pos('ase', i), cond_pos('aee', i), cond_pos_pos( ['ase', 'aee'], i ) )
		)
		# tse & tee & (!ase | !aee)
		st['err'][i] = And(
			cond_pos('tse', i), cond_pos('tee', i), cond_pos_pos( ['tse', 'tee'], i ),
			#Or( cond_neg('ase', i), cond_neg('aee', i) )
			Or( 
				#cond_neg('ase', i), cond_neg('aee', i), 
				cond_pos_neg(['tse', 'tee'], 'ase', 0), cond_pos_neg(['tse', 'tee'], 'aee', 0)
			)
		)
	
	#print st
	return st

def qfree_encode(sup, sigma, step, param_time, param_cond):
	# st[q][i][j] <=> (s_i^q & c_i^q[j]) <=> (q, j) is reachable at step i
	st = { s : { i : False for i in range(step+1) } for s in SUP_STATE }
	#print st
	
	def cond_pos(cond, step):
		clause = []
		for i in list(sup.var_dic.values()):
			v = sup.SYS_VAR[i]
			if sigma[ v[step] ] is not None:
				if sigma[ v[step] ] == True:
					clause.append( param_cond[cond][i][0] != neg )
				elif sigma[ v[step] ] == False:
					clause.append( param_cond[cond][i][0] != pos )
		return And(clause)
		
	def cond_neg(cond, step):
		clause = []
		for i in list(sup.var_dic.values()):
			v = sup.SYS_VAR[i]
			if sigma[ v[step] ] is not None:
				if sigma[ v[step] ] == True:
					clause.append( param_cond[cond][i][0] == neg )
				elif sigma[ v[step] ] == False:
					clause.append( param_cond[cond][i][0] == pos )
		return Or(clause)

	def cond_pos_neg(cpos_list, cneg, step):
		clause = []
		for i in list(sup.var_dic.values()):
			v = sup.SYS_VAR[i]
			if sigma[ v[step] ] is None:
				clause.append( And(
					[ 
						param_cond[ cpos_list[j] ][i][0] != param_cond[cneg][i][0] 
						for j in range( len(cpos_list) )
					] + [ param_cond[cneg][i][0] != dc ]
				) )
			elif sigma[ v[step] ] == True:
				clause.append( param_cond[cneg][i][0] == neg )
			elif sigma[ v[step] ] == False:
				clause.append( param_cond[cneg][i][0] == pos )
		return Or(clause)
		
	def cond_pos_pos(cond_list, step):
		clause = []

		if len(cond_list) > 2:
			for (i, j) in itertools.combinations( list(range(len(cond_list))), 2 ):
				clause.append( cond_pos_pos( [ cond_list[i], cond_list[j] ], step ) )
			return And(clause)
		
		if len(cond_list) == 2:
			for i in list(sup.var_dic.values()):
				v = sup.SYS_VAR[i]
				if sigma[ v[step] ] is None:
					clause.append( Or(
						param_cond[ cond_list[0] ][i][0] == param_cond[ cond_list[1] ][i][0],
						param_cond[ cond_list[0] ][i][0] == dc,
						param_cond[ cond_list[1] ][i][0] == dc
					) )
			return And(clause)
	
	"""
	def cond_neg_neg_p(cond, step):
		clause = []
		for i in sup.var_dic.values():
			v = sup.SYS_VAR[i]
			if sigma[ v[step] ] is None:
				clause.append( param_cond[cond][i][0] == pos )
		return Or(clause)
	
	def cond_neg_neg_n(cond, step):
		clause = []
		for i in sup.var_dic.values():
			v = sup.SYS_VAR[i]
			if sigma[ v[step] ] is None:
				clause.append( param_cond[cond][i][0] == neg )
		return Or(clause)
	"""

	def cond_pos_neg_neg(cpos, neg1, neg2, step):
		clause = []
		for i in list(sup.var_dic.values()):
			for j in list(sup.var_dic.values()):
				v1 = sup.SYS_VAR[i]
				v2 = sup.SYS_VAR[j]
				subclause = []
				if sigma[ v1[step] ] == True:
					subclause.append( param_cond[neg1][i][0] == neg )
				elif sigma[ v1[step] ] == False:
					subclause.append( param_cond[neg1][i][0] == pos )
				else:
					subclause.append( And(
						param_cond[cpos][i][0] != param_cond[neg1][i][0],
						param_cond[neg1][i][0] != dc 
					) )
				if sigma[ v2[step] ] == True:
					subclause.append( param_cond[neg2][j][0] == neg )
				elif sigma[ v2[step] ] == False:
					subclause.append( param_cond[neg2][j][0] == pos )
				else:
					subclause.append( And(
						param_cond[cpos][j][0] != param_cond[neg2][j][0],
						param_cond[neg2][j][0] != dc 
					) )
				if i == j and sigma[ v1[step] ] is None:
					subclause.append( param_cond[neg1][i][0] == param_cond[neg2][j][0] )
				clause.append( And(subclause) )
		return Or(clause)

	def cond_neg_neg(neg1, neg2, step):
		clause = []
		for i in list(sup.var_dic.values()):
			for j in list(sup.var_dic.values()):
				v1 = sup.SYS_VAR[i]
				v2 = sup.SYS_VAR[j]
				subclause = []
				if i == j and sigma[ v1[step] ] is None:
					clause.append( And(
						param_cond[neg1][i][0] == param_cond[neg2][j][0],
						param_cond[neg1][i][0] != dc
					) )
				else:
					if sigma[ v1[step] ] == True:
						subclause.append( param_cond[neg1][i][0] == neg )
					elif sigma[ v1[step] ] == False:
						subclause.append( param_cond[neg1][i][0] == pos )
					else:
						subclause.append( param_cond[neg1][i][0] != dc )
					if sigma[ v2[step] ] == True:
						subclause.append( param_cond[neg2][j][0] == neg )
					elif sigma[ v2[step] ] == False:
						subclause.append( param_cond[neg2][j][0] == pos )
					else:
						subclause.append( param_cond[neg2][j][0] != dc )
					clause.append( And(subclause) )	
		return Or(clause)

	def cond_neg3(neg1, neg2, neg3, step):
		clause = []
		for i in list(sup.var_dic.values()):
			for j in list(sup.var_dic.values()):
				for k in list(sup.var_dic.values()):
					v1 = sup.SYS_VAR[i]
					v2 = sup.SYS_VAR[j]
					v3 = sup.SYS_VAR[k]
					subclause = []
					if i == j and sigma[ v1[step] ] is None:
						subclause.append( param_cond[neg1][i][0] == param_cond[neg2][j][0] )
					if j == k and sigma[ v2[step] ] is None:
						subclause.append( param_cond[neg2][j][0] == param_cond[neg3][k][0] )
					if k == i and sigma[ v3[step] ] is None:
						subclause.append( param_cond[neg3][k][0] == param_cond[neg1][i][0] )
					if sigma[ v1[step] ] == True:
						subclause.append( param_cond[neg1][i][0] == neg )
					elif sigma[ v1[step] ] == False:
						subclause.append( param_cond[neg1][i][0] == pos )
					else:
						subclause.append( param_cond[neg1][i][0] != dc )
					if sigma[ v2[step] ] == True:
						subclause.append( param_cond[neg2][j][0] == neg )
					elif sigma[ v2[step] ] == False:
						subclause.append( param_cond[neg2][j][0] == pos )
					else:
						subclause.append( param_cond[neg2][j][0] != dc )
					if sigma[ v3[step] ] == True:
						subclause.append( param_cond[neg3][k][0] == neg )
					elif sigma[ v3[step] ] == False:
						subclause.append( param_cond[neg3][k][0] == pos )
					else:
						subclause.append( param_cond[neg3][k][0] != dc )
					clause.append( And(subclause) )	
		return Or(clause)

	# Initialization(q0)
	st['init'][0] = Or(
		# !tse
		cond_neg('tse', 0),
		# tse & !tee & tmax = 0
		And( 
			cond_pos('tse', 0), cond_pos_neg(['tse'], 'tee', 0), 
			param_time['tmax'] == 0 
		),
		# tse & tee & tmin = 0 & ase & lmin = 0 & aee & amin = 0
		And(
			cond_pos('tse', 0), cond_pos('tee', 0), param_time['tmin'] == 0, 
			cond_pos('ase', 0), param_time['lmin'] == 0, 
			cond_pos('aee', 0), param_time['amin'] == 0,
			cond_pos_pos( ['tse', 'tee', 'ase', 'aee'], 0 )
		)
	)
	
	# tse & (!tee | tmin > 0) & tmax > 0
	st['trg'][0] = And(
		cond_pos('tse', 0), Or( cond_pos_neg(['tse'], 'tee', 0), param_time['tmin'] != 0 ),
		param_time['tmax'] != 0
	)
	
	# tse & tee & tmin = 0 & (!ase | lmin != 0)
	st['loc'][0] = And(
		cond_pos('tse', 0), cond_pos('tee', 0), param_time['tmin'] == 0,
		Or( cond_pos_neg( ['tse', 'tee'], 'ase', 0 ), param_time['lmin'] != 0 ),
		cond_pos_pos( ['tse', 'tee'], 0 )
	)
	
	# tse & tee & tmin = 0 & ase & lmin = 0 & (!aee | amin > 0)
	st['act'][0] = And(
		cond_pos('tse', 0), cond_pos('tee', 0), param_time['tmin'] == 0, 
		cond_pos('ase', 0), param_time['lmin'] == 0, 
		Or( cond_pos_neg( ['tse', 'tee', 'ase'], 'aee', 0 ), param_time['amin'] != 0 ),
		cond_pos_pos( ['tse', 'tee', 'ase'], 0 )
	)
	
	st['err'][0] = Or(
		# tse & tee & tmin = 0 & !ase & lmax = 0
		And(
			cond_pos('tse', 0), cond_pos('tee', 0), param_time['tmin'] == 0, 
			cond_pos_neg( ['tse', 'tee'], 'ase', 0 ), param_time['lmax'] == 0,
			cond_pos_pos( ['tse', 'tee'], 0 )
		),
		# tse & tee & tmin = 0 & ase & lmin = 0 & !aee & amax = 0
		And(
			cond_pos('tse', 0), cond_pos('tee', 0), param_time['tmin'] == 0, 
			cond_pos('ase', 0), param_time['lmin'] == 0, 
			cond_pos_neg( ['tse', 'tee', 'ase'], 'aee', 0 ), param_time['amax'] == 0,
			cond_pos_pos( ['tse', 'tee', 'ase'], 0 )
		)
	)
	
	# Transition(qi, qi+1)
	for i in range(1, step+1):
		st['init'][i] = Or(
			And(
				# previous state is init (init has no counter, always cnt == 0)
				st['init'][i-1],
				Or(
					# !tse
					cond_neg('tse', i),
					# tse & !tee & tmax = 0
					And( 
						cond_pos('tse', i), cond_pos_neg(['tse'], 'tee', i), 
						param_time['tmax'] == 0 
					),
					# tse & tee & tmin = 0 & ase & lmin = 0 & aee & amin = 0
					And(
						cond_pos('tse', i), cond_pos('tee', i), param_time['tmin'] == 0, 
						cond_pos('ase', i), param_time['lmin'] == 0, 
						cond_pos('aee', i), param_time['amin'] == 0,
						cond_pos_pos( ['tse', 'tee', 'ase', 'aee'], i )
					)
				)
			),
			And(
				# previous state is trg
				st['trg'][i-1],
				Or(
					# !tee & !tc & !tse
					cond_neg3('tee', 'tc', 'tse', i),
					#Or(
					#	And(
					#		Or( cond_neg('tee', i), cond_neg_neg_p('tee', i) ),
					#		Or( cond_neg('tc', i), 0 == param_time['tmax'], cond_neg_neg_p('tc', i) ),
					#		Or( cond_neg('tse', i), cond_neg_neg_p('tse', i) )
					#	),
					#	And(
					#		Or( cond_neg('tee', i), cond_neg_neg_n('tee', i) ),
					#		Or( cond_neg('tc', i), 0 == param_time['tmax'], cond_neg_neg_n('tc', i) ),
					#		Or( cond_neg('tse', i), cond_neg_neg_n('tse', i) )
					#	)
					#),
					# (tee & cnt + 1 >= tmin ) & (ase & 0 == lmin ) & (aee & 0 == amin) & (!tse | 0 == tmin)
					And(
						cond_pos('tee', i),
						cond_pos('ase', i), 
						cond_pos('aee', i), 
						cond_pos_pos( ['tee', 'ase', 'aee'], i ),
						cond_pos_neg( ['tee', 'ase', 'aee'], 'tse', i )
					)
				)
			),
			And(
				# previous state is loc
				st['loc'][i-1],
				Or(
					# ase & cnt + 1 >= lmin & aee & 0 == amin & tse & (!tee | tmin > 0) & 0 == tmax
					And(
						cond_pos('ase', i),
						cond_pos('aee', i),
						cond_pos('tse', i), 
						cond_pos_neg( ['ase', 'aee', 'tse'], 'tee', i ),
						param_time['tmax'] == 0, cond_pos_pos(['ase', 'aee', 'tse'], i)
					),
					# ase & cnt + 1 >= lmin & aee & 0 == amin & tse & tee & tmin == 0 & 0 == lmin
					And(
						cond_pos('ase', i), 
						cond_pos('aee', i), 
						cond_pos('tse', i), cond_pos('tee', i),
						cond_pos_pos(['ase', 'aee', 'tse', 'tee'], i)
					),
					# ase & cnt + 1 >= lmin & aee & 0 == amin & !tse
					And(
						cond_pos('ase', i),
						cond_pos('aee', i),
						cond_pos_neg( ['ase', 'aee'], 'tse', i ),
						cond_pos_pos(['ase', 'aee'], i)
					)
				)
			),
			And(
				# previous state is act
				st['act'][i-1],
				Or(
					# aee & cnt + 1 >= amin & tse & !tee & 0 == tmax
					And(
						cond_pos('aee', i),
						cond_pos('tse', i), cond_pos_neg(['aee', 'tse'], 'tee', i), 
						param_time['tmax'] == 0, cond_pos_pos(['aee', 'tse'], i)
					),
					# aee & cnt + 1 >= amin & tse & tee & 0 == tmin & ase & 0 == lmin  & amin == 0
					And(
						cond_pos('aee', i),
						cond_pos('tse', i), cond_pos('tee', i),
						cond_pos('ase', i), 
						cond_pos_pos(['aee', 'tse', 'tee', 'ase'], i)
					),
					# aee & cnt + 1 >= amin & !tse
					And(
						cond_pos('aee', i), 
						cond_pos_neg(['aee'], 'tse', i)
					)
				)
			)
		)
		
		st['trg'][i] = Or(
			And(
				# previous state is init (init has no counter, always cnt == 0)
				st['init'][i-1],
				# tse & (!tee | tmin > 0) & tmax > 0
				cond_pos('tse', i), cond_pos_neg(['tse'], 'tee', i),
				param_time['tmax'] != 0
			),
			And(
				# previous state is trg
				st['trg'][i-1],
				Or(
					# !tee & !tc & tse
					cond_pos_neg_neg('tse', 'tee', 'tc', i),
					#And(
					#	Or( cond_neg('tee', i), cond_neg_neg_p('tee', i) ),
					#	Or( cond_neg('tc', i), cond_neg_neg_p('tc', i) ),
					#	cond_pos('tse', i)
					#),
					#And(
					#	Or( cond_neg('tee', i), cond_neg_neg_n('tee', i) ),
					#	Or( cond_neg('tc', i), cond_neg_neg_n('tc', i) ),
					#	cond_pos('tse', i)
					#),
					# tee & cnt + 1 >= tmin & ase & lmin = 0 & aee & amin == 0 & tse & tmin > 0
					# !tee & tc
					And(
						cond_pos_neg(['tc'], 'tee', i), cond_pos('tc', i)
					)
				)
			),
			And(
				# previous state is loc
				st['loc'][i-1],
				# ase & aee & tse & !tee & 0 != tmax
				And(
					cond_pos('ase', i),
					cond_pos('aee', i),
					cond_pos('tse', i), cond_pos_pos(['ase', 'aee', 'tse'], i),
					cond_pos_neg(['ase', 'aee', 'tse'], 'tee', i),
					param_time['tmax'] != 0
				)
			),
			And(
				# previous state is act
				st['act'][i-1],
				# aee & tse & !tee & 0 != tmax
				And(
					cond_pos('aee', i), 
					cond_pos('tse', i), cond_pos_pos(['aee', 'tse'], i),
					cond_pos_neg(['aee', 'tse'], 'tee', i),
					param_time['tmax'] != 0
				)
			)
		)
		
		#for j in range(1, i+1):
		#	st['trg'][i][j] = Or(
		#		And(
		#			st['trg'][i-1][j-1],
		#			# (!tee | j < tmin) & j < tmax & tc
		#			Or( cond_pos_neg(['tc'], 'tee', i), param_time['tmin'] > j ),
		#			param_time['tmax'] > j, cond_pos('tc', i)
		#		)
		#	)
			
		st['loc'][i] = Or(
			And(
				# previous state is init (init has no counter, always cnt == 0)
				st['init'][i-1],
				# tse & tee & tmin == 0 & (!ase | lmin > 0) & lmax > 0
				cond_pos('tse', i), cond_pos('tee', i), 
				cond_pos_neg(['tse', 'tee'], 'ase', i),
				param_time['lmax'] != 0
			),
			And(
				# previous state is trg
				st['trg'][i-1],
				# tee & j + 1 >= tmin & (!ase | lmin > 0) & lmax > 0
				cond_pos('tee', i),
				cond_pos_neg(['tee'], 'ase', i),
				param_time['lmax'] != 0
			),
			And(
				# previous state is loc
				st['loc'][i-1],
				# !ase
				cond_neg('ase', i)
			),
			And(
				# previous state is act
				st['act'][i-1],
				# aee & tse & tee & !ase & lmax != 0
				cond_pos('aee', i), cond_pos('tse', i), cond_pos('tee', i),
				cond_pos_pos(['aee', 'tse', 'tee'], i), 
				cond_pos_neg(['aee', 'tse', 'tee'], 'ase', i), param_time['lmax'] != 0
			)
		)
		
		st['act'][i] = Or(
			And(
				# previous state is init (init has no counter, always cnt == 0)
				st['init'][i-1],
				# tse & tee & ase & !aee & amax != 0
				cond_pos('tse', i), cond_pos('tee', i), cond_pos('ase', i),
				cond_pos_pos(['tse', 'tee', 'ase'], i),
				cond_pos_neg(['tse', 'tee', 'ase'], 'aee', i),
				param_time['amax'] != 0
			),
			And(
				# previous state is trg
				st['trg'][i-1],
				# tee & ase & !aee & amax != 0
				cond_pos('tee', i), cond_pos('ase', i),
				cond_pos_pos(['tee', 'ase'], i),
				cond_pos_neg(['tee', 'ase'], 'aee', i),
				param_time['amax'] != 0
			),
			And(
				# previous state is loc
				st['loc'][i-1],
				# ase & !aee & amax != 0
				cond_pos('ase', i),
				cond_pos_neg(['ase'], 'aee', i),
				param_time['amax'] != 0
			),
			And(
				# previous state is act
				st['act'][i-1],
				# !aee & ac
				cond_pos_neg(['ac'], 'aee', i), cond_pos('ac', i)
			)
		)
		
		st['err'][i] = Or(
			And(
				# previous state is init (init has no counter, always cnt == 0)
				st['init'][i-1],
				Or(
					# tse & tee & !ase & lmax = 0
					And(
						cond_pos('tse', i), cond_pos('tee', i), cond_pos_pos(['tse', 'tee'], i),
						cond_pos_neg(['tse', 'tee'], 'ase', i), param_time['lmax'] == 0
					),
					# tse & tee & ase & !aee & amax = 0
					And(
						cond_pos('tse', i), cond_pos('tee', i), cond_pos('ase', i),
						cond_pos_pos(['tse', 'tee', 'ase'], i),
						cond_pos_neg(['tse', 'tee', 'ase'], 'aee', i), param_time['amax'] == 0
					)
				)
			),
			And(
				# previous state is trg
				st['trg'][i-1],
				Or(
					# tee & !ase & lmax = 0
					And(
						cond_pos('tee', i), cond_pos_neg(['tee'], 'ase', i), 
						param_time['lmax'] == 0
					),
					# tee & ase & !aee & amax = 0
					And(
						cond_pos('tee', i), cond_pos('ase', i),
						cond_pos_pos(['tee', 'ase'], i),
						cond_pos_neg(['tee', 'ase'], 'aee', i), param_time['amax'] == 0
					)
				)
			),
			And(
				# previous state is loc
				st['loc'][i-1],
				# ase & !aee & amax = 0
				cond_pos('ase', i), cond_pos_neg(['ase'], 'aee', i), param_time['amax'] == 0
			),
			And(
				# previous state is act
				st['act'][i-1],
				Or(
					# !aee & !ac
					cond_neg_neg('aee', 'ac', i),
					#And(
					#	Or( cond_neg('aee', i), cond_neg_neg_p('aee', i) ),
					#	Or( cond_neg('ac', i), cond_neg_neg_p('ac', i) )
					#),
					#And(
					#
					#),
					# aee & tse & tee & !ase & lmax = 0
					And(
						cond_pos('aee', i), cond_pos('tse', i), cond_pos('tee', i),
						cond_pos_pos(['aee', 'tse', 'tee'], i),
						cond_pos_neg(['aee', 'tse', 'tee'], 'ase', i),
						param_time['lmax'] == 0
					)
				)
			),
			# previous state is err
			st['err'][i-1]
		)

	#print st
	return st

def qfree_encode_1step(sup, step, param_time, param_cond, st):
	for s in SUP_STATE:
		st[s][step] = False
	
	def cond_pos(cond, step):
		clause = []
		
		for i in list(sup.var_dic.values()):
			v = sup.SYS_VAR[i]
			clause.append( If(
				v[step],
				param_cond[cond][i][0] != neg,
				param_cond[cond][i][0] != pos
			))

		return And(clause)
		
	def cond_neg(cond, step):
		clause = []
		
		for i in list(sup.var_dic.values()):
			v = sup.SYS_VAR[i]
			clause.append( If(
				v[step],
				param_cond[cond][i][0] == neg,
				param_cond[cond][i][0] == pos
			))

		return Or(clause)
	
	i = step
	st['init'][i] = Or(
		And(
			# previous state is init
			st['init'][i-1],
			Or(
				# !tse
				cond_neg('tse', i),
				# tse & !tee & tmax = 0
				And( cond_pos('tse', i), cond_neg('tee', i), param_time['tmax'] == 0 ),
				# tse & tee & tmin = 0 & ase & lmin = 0 & aee & amin = 0
				And(
					cond_pos('tse', i), cond_pos('tee', i), param_time['tmin'] == 0, 
					cond_pos('ase', i), param_time['lmin'] == 0, 
					cond_pos('aee', i), param_time['amin'] == 0
				)
			)
		),
		And(
			# previous state is trg
			st['trg'][i-1],
			Or(
				# !tee & !tc & !tse
				And( cond_neg('tee', i), cond_neg('tc', i), cond_neg('tse', i) ),
				# (tee & cnt + 1 >= tmin ) & (ase & 0 == lmin ) & (aee & 0 == amin) & (!tse | 0 != tmin)
				And( cond_pos('tee', i), cond_pos('ase', i),  cond_pos('aee', i), cond_neg('tse', i) )
			)
		),
		And(
			# previous state is loc
			st['loc'][i-1],
			Or(
				# ase & cnt + 1 >= lmin & aee & 0 == amin & tse & (!tee | tmin > 0) & 0 == tmax
				And(
					cond_pos('ase', i), cond_pos('aee', i), cond_pos('tse', i), cond_neg('tee', i),
					param_time['tmax'] == 0
				),
				# ase & cnt + 1 >= lmin & aee & 0 == amin & tse & tee & tmin == 0 & 0 == lmin
				And( cond_pos('ase', i), cond_pos('aee', i), cond_pos('tse', i), cond_pos('tee', i) ),
				# ase & cnt + 1 >= lmin & aee & 0 == amin & !tse
				And( cond_pos('ase', i), cond_pos('aee', i), cond_neg('tse', i) )
			)
		),
		And(
			# previous state is act
			st['act'][i-1],
			Or(
				# aee & cnt + 1 >= amin & tse & !tee & 0 == tmax
				And( cond_pos('aee', i), cond_pos('tse', i),  cond_neg('tee', i), param_time['tmax'] == 0 ),
				# aee & cnt + 1 >= amin & tse & tee & 0 == tmin & ase & 0 == lmin  & amin == 0
				And( cond_pos('aee', i), cond_pos('tse', i), cond_pos('tee', i), cond_pos('ase', i) ),
				# aee & cnt + 1 >= amin & !tse
				And( cond_pos('aee', i),  cond_neg('tse', i) )
			)
		)
	)
	
	st['trg'][i] = Or(
		And(
			# previous state is init
			st['init'][i-1],
			# tse & (!tee | tmin > 0) & tmax > 0
			cond_pos('tse', i), cond_neg('tee', i), param_time['tmax'] != 0
		),
		And(
			# previous state is trg
			st['trg'][i-1],
			Or(
				# !tee & !tc & tse
				#cond_pos_neg_neg('tse', 'tee', 'tc', i),
				And( cond_neg('tee', i), cond_neg('tc', i), cond_pos('tse', i) ),
				# !tee & tc
				And( cond_neg('tee', i), cond_pos('tc', i) )
			)
		),
		And(
			# previous state is loc
			st['loc'][i-1],
			# ase & aee & tse & !tee & 0 != tmax
			And(
				cond_pos('ase', i), cond_pos('aee', i), cond_pos('tse', i), 
				cond_neg('tee', i), param_time['tmax'] != 0
			)
		),
		And(
			# previous state is act
			st['act'][i-1],
			# aee & tse & !tee & 0 != tmax
			And(
				cond_pos('aee', i), cond_pos('tse', i), cond_neg('tee', i),
				param_time['tmax'] != 0
			)
		)
	)
		
	st['loc'][i] = Or(
		And(
			# previous state is init 
			st['init'][i-1],
			# tse & tee & tmin == 0 & (!ase | lmin > 0) & lmax > 0
			cond_pos('tse', i), cond_pos('tee', i), cond_neg('ase', i), param_time['lmax'] != 0
		),
		And(
			# previous state is trg
			st['trg'][i-1],
			# tee & j + 1 >= tmin & (!ase | lmin > 0) & lmax > 0
			cond_pos('tee', i), cond_neg('ase', i), param_time['lmax'] != 0
		),
		And(
			# previous state is loc
			st['loc'][i-1],
			# !ase
			cond_neg('ase', i)
		),
		And(
			# previous state is act
			st['act'][i-1],
			# aee & tse & tee & !ase & lmax != 0
			cond_pos('aee', i), cond_pos('tse', i), cond_pos('tee', i),
			cond_neg('ase', i), param_time['lmax'] != 0
		)
	)
	
	st['act'][i] = Or(
		And(
			# previous state is init
			st['init'][i-1],
			# tse & tee & ase & !aee & amax != 0
			cond_pos('tse', i), cond_pos('tee', i), cond_pos('ase', i),
			cond_neg('aee', i), param_time['amax'] != 0
		),
		And(
			# previous state is trg
			st['trg'][i-1],
			# tee & ase & !aee & amax != 0
			cond_pos('tee', i), cond_pos('ase', i), cond_neg('aee', i),
			param_time['amax'] != 0
		),
		And(
			# previous state is loc
			st['loc'][i-1],
			# ase & !aee & amax != 0
			cond_pos('ase', i), cond_neg('aee', i), param_time['amax'] != 0
		),
		And(
			# previous state is act
			st['act'][i-1],
			# !aee & ac
			cond_neg('aee', i), cond_pos('ac', i)
		)
	)
	
	st['err'][i] = Or(
		And(
			# previous state is init
			st['init'][i-1],
			Or(
				# tse & tee & !ase & lmax = 0
				And(
					cond_pos('tse', i), cond_pos('tee', i), cond_neg('ase', i), 
					param_time['lmax'] == 0
				),
				# tse & tee & ase & !aee & amax = 0
				And(
					cond_pos('tse', i), cond_pos('tee', i), cond_pos('ase', i),
					cond_neg('aee', i), param_time['amax'] == 0
				)
			)
		),
		And(
			# previous state is trg
			st['trg'][i-1],
			Or(
				# tee & !ase & lmax = 0
				And( cond_pos('tee', i), cond_neg('ase', i), param_time['lmax'] == 0 ),
				# tee & ase & !aee & amax = 0
				And(
					cond_pos('tee', i), cond_pos('ase', i), cond_neg('aee', i), 
					param_time['amax'] == 0
				)
			)
		),
		And(
			# previous state is loc
			st['loc'][i-1],
			# ase & !aee & amax = 0
			cond_pos('ase', i), cond_neg('aee', i), param_time['amax'] == 0
		),
		And(
			# previous state is act
			st['act'][i-1],
			Or(
				# !aee & !ac
				#cond_neg_neg('aee', 'ac', i),
				And( cond_neg('aee', i), cond_neg('ac', i) ),
				# aee & tse & tee & !ase & lmax = 0
				And(
					cond_pos('aee', i), cond_pos('tse', i), cond_pos('tee', i),
					cond_neg('ase', i), param_time['lmax'] == 0
				)
			)
		),
		# previous state is err
		st['err'][i-1]
	)
	
	return st

def qfree_test(sup, sigma, step, param_time, param_cond, s, copy):
	for r in sup.REQ_SET:
		if (sigma[ r.st[step] ] is None) or (sigma[ r.cnt[step] ] is None):
			print('error at qfree_zinf.qfree_test(): the given trace is not a single run')
			print_state(sup, sigma, step)
			sys.exit()
	
	"""#for r in sup.REQ_SET:
	for i, r in enumerate(copy.REQ_SET):
		r.model(0, step+1)
		#s.add( r.st[step] == sigma[ r.st[step] ] )
		s.add( r.st[step] == sigma[ sup.REQ_SET[i].st[step] ] )
		#s.add( r.cnt[step] == sigma[ r.cnt[step] ] )
		s.add( r.cnt[step] == sigma[ sup.REQ_SET[i].cnt[step] ] )
		s.add( r.formula[step+1] )
		s.add( r.st[step+1] != supreq.err )"""
	
	formula = []
	for i, r in enumerate(copy.REQ_SET):
		r.model(0, step+1)
		formula.append( r.st[step] == sigma[ sup.REQ_SET[i].st[step] ] )
		formula.append( r.cnt[step] == sigma[ sup.REQ_SET[i].cnt[step] ] )
		formula.append( And(r.formula[step+1]) )
		formula.append( r.st[step+1] != supreq.err )

	st = qfree_encode(sup, sigma, step, param_time, param_cond)
	st_1step_ahead = qfree_encode_1step(copy, step+1, param_time, param_cond, st)
	s.add( Or(
		# generated requirement rejects the trace
		And( 
			st['err'][step], Not( st['init'][step] ), Not( st['trg'][step] ), 
			Not( st['loc'][step] ), Not( st['act'][step] ) 
		),
		# there exists a 1 step extension satisfying all requirements
		And(
			And(formula), Not( st_1step_ahead['err'][step+1] )
		)
	))
	
	"""if s.check() == sat:
		model = s.model()
		print_req_gen(sup, model, param_time, param_cond)
		print
		print_trace(copy, model, step+1)
		print
		print_state(copy, model, step+1)

	sys.exit()"""
	
	return s

"""
if __name__ == "__main__":
	if len(sys.argv) < 2:
		print "Error: no input file"
		sys.exit()

	arg = sys.argv[1]
	if len(arg) > 3:
		if arg[len(arg)-3:len(arg)] == '.py':
			arg = arg[:len(arg)-3]

	if os.path.isfile(arg + '.py') == False:
		print "Error: file not found"
		sys.exit()

	mod = import_module(arg)
	sup = supreq.SUPInput(mod)

	print "Input requirements:"
	for r in sup.REQ_SET:
		print "    Req." + str(r.id) + ":",
		reqgen.print_req(sup, r.req)
	print

	sigma, step = bounded_rt.bounded_check(sup, sup.REQ_SET, sup.ALPHA)
	if sigma == []:
		print "Given requirements are consistent: no need to fix"
		sys.exit()

	print "Given requirements are inconsistent."

	if VERBOSE: 
		print '\nTrace found:'
		reqgen.print_trace_b(sup, sigma, step)
		print

	# SUP parameters Tmin, Tmax, Lmin, Lmax, Amin, Amax
	param_time = { t : Int(t) for t in supreq.sup_time }

	# SUP parameters TSE, TC, TEE, ASE, AC, AEE
	param_cond = {}
	for cond in supreq.sup_cond:
		lt = [None] * len(sup.var_dic)
		for v, i in sup.var_dic.items():
			lt[i] = [ 
				Const(cond + '_' + str(j) + '_' + v, Literal) 
			for j in range(MAX_CLAUSE) ]
		param_cond[cond] = lt

	# debug
	#print "step = " + str(step)
	#print param_time
	#print param_cond
	#print param_cond['tse']
	# debug

	st = qfree_encode(sup, sigma, step, param_time, param_cond)
	#st = qfree_untimed(sup, sigma, step, param_time, param_cond)
	
	s = Solver()
	s.add(
		0 == param_time['tmin'],
		0 == param_time['lmin'],
		0 == param_time['amin'],
		Or( 0 == param_time['tmax'], -1 == param_time['tmax'] ),
		Or( 0 == param_time['lmax'], -1 == param_time['lmax'] ),
		Or( 0 == param_time['amax'], -1 == param_time['amax'] )
	)
	s.add( Not( st['err'][0] ) )
	s.add( And( Not( st['init'][step] ), Not( st['trg'][step] ), Not( st['loc'][step] ), Not( st['act'][step] ) ) )
	
	if s.check() == unsat:
		print "Generation failed: no solution"
	else:
		model = s.model()
		reqgen.print_req_gen(sup, model, param_time, param_cond)
		req_gen = reqgen.decode_req(sup, model, param_time, param_cond)

		sigma, step = bounded_rt.bounded_check(
			sup, 
			sup.REQ_SET + [ supreq.Requirement( sup, req_gen, len(sup.REQ_SET) ) ], 
			sup.ALPHA,
			True
		)
"""








