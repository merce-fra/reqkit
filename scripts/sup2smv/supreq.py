from z3 import *
from importlib import import_module
import argparse
import subprocess
import shutil
import sys
import os
import copy

State, (init, trg, loc, act, err) = EnumSort( 'State', ['init', 'trg', 'loc', 'act', 'err'] )
sup_dic = { 
  'tse' : 0, 'tc' : 1, 'tee' : 2, 'tmin' : 3, 'tmax' : 4, 'lmin' : 5, 'lmax' :6 ,
  'ase' :7 , 'ac' : 8, 'aee' : 9, 'amin' : 10, 'amax' : 11 
}
sup_cond = { 'tse' : 0, 'tc' : 1, 'tee' : 2, 'ase' :7 , 'ac' : 8, 'aee' : 9 }
sup_time = { 'tmin' : 3, 'tmax' : 4, 'lmin' : 5, 'lmax' :6 , 'amin' : 10, 'amax' : 11 }
sup_rev = { v : k for k, v in list(sup_dic.items()) }

STEP_SUFFIX = '__'
NUSMV_CALL = ['NuSMV']
NUSMV_BASE = './template.smv'
NUSMV_FILE = './test.smv'
NUSMV_TRUE = '-- specification AG (some_err | EX !some_err)  is true'
NUSMV_FALSE = '-- specification AG (some_err | EX !some_err)  is false'
NUSMV_TRACE = '-> State:'

class SUPInput:
  def __init__(self, mod):
    self.ALPHA = mod.ALPHA
    self.BETA = mod.BETA
    self.MAX_PTRACE = mod.MAX_PTRACE
    self.var_dic = {}
    self.SYS_VAR = []
    self.REQ_SET = [ Requirement(self, r, i) for i, r in enumerate(mod.REQ_SET) ]
    self.COND_INIT = []
    self.SYS_VAR_RAW = []

    # extract system variables from requirements and add them to var_dic
    for req in self.REQ_SET:
      for cond in sup_cond:
        self.extract_var( req.req[sup_dic[cond]] )

    for i in range( len(self.var_dic) ):
      self.SYS_VAR.append([])
    
    self.init_decl(mod.COND_INIT)

  def extract_var(self, symbol):
    if type(symbol) is bool:
      return
    elif symbol.num_args() > 0:
      for i in range( symbol.num_args() ):
        self.extract_var( symbol.arg(i) )
      return
    elif str(symbol.decl()) == 'True' and symbol.decl().name() == 'true':
      return
    elif str(symbol.decl()) == 'False' and symbol.decl().name() == 'false':
      return
    else:
      var = symbol.decl().name()
      if var not in self.var_dic:
        self.var_dic[var] = len(self.var_dic)
        self.SYS_VAR_RAW.append(symbol)
      return

  def sys_var_decl(self, step):
    for v, i in list(self.var_dic.items()):
      if len( self.SYS_VAR[i] ) <= step:
        for j in range( len( self.SYS_VAR[i] ), step + 1 ):
          self.SYS_VAR[self.var_dic[v]].append( Bool( v + STEP_SUFFIX + str(j) ) )

  def init_decl(self, cond_init, suffix = ''):
    self.sys_var_decl(0)
    for cond in cond_init:
      if cond.decl().name() == '=':
        name = cond.arg(0).decl().name()

        # If init_decl() is called from duplicate(), remove step suffix and add given suffix
        if len(suffix) > 0:
          name = name[:len(name) - len(STEP_SUFFIX + '0')] + suffix

        val = cond.arg(1)
        if val.sort() == BoolSort():
          if val.decl().name() == 'true': val = True
          else: val = False
        else:
          print('error: type \"' + val.sort().name() + '\" is not supported')
          sys.exit()

        self.COND_INIT.append( self.SYS_VAR[ self.var_dic[name] ][0] == val )
      else:
        print('at supreq.init_decl()')
        print('error: operator \"' +  cond.decl().name() + '\" is not supported for initialization')
        sys.exit()

  def duplicate(self, suffix):
    dup = copy.copy(self)
    dup.var_dic = {}
    dup.SYS_VAR = [ [] for i in range(len(self.var_dic)) ]
    dup.REQ_SET = []
    dup.COND_INIT = []
    for v, i in list(self.var_dic.items()):
      dup.var_dic[v + suffix] = i
    dup.init_decl(self.COND_INIT, suffix)
    dup.SYS_VAR_RAW = [ [] for i in range(len(dup.var_dic)) ]
    for v, i in list(dup.var_dic.items()):
      dup.SYS_VAR_RAW[i] = Bool(v)
    return dup

  def duplicate_req(self, dup, suffix, idx):
    dup.REQ_SET = [ 
      Requirement(dup, r, i + idx, suffix) 
      for i, r in enumerate( [ r.req for r in self.REQ_SET ] ) 
    ]
    return dup, idx + len(dup.REQ_SET)

  def duplicate_all(self, suffix, idx):
    dup = self.duplicate(suffix)
    return self.duplicate_req(dup, suffix, idx)

  def conv_exp_smv(self, symbol):
    res = ''
    if type(symbol) is bool:
      if symbol: res = 'TRUE'
      else: res = 'FALSE'
    elif str(symbol.decl()) == 'True' and symbol.decl().name() == 'true':
      res = 'TRUE'
    elif str(symbol.decl()) == 'False' and symbol.decl().name() == 'false':
      res = 'FALSE'
    elif symbol.num_args() == 0:
      res = symbol.decl().name()
    else:
      if symbol.decl().name() == 'not':
        res = '!' + self.conv_exp_smv(symbol.arg(0))
      else:
        child = []
        for i in range(symbol.num_args()):
          child.append(self.conv_exp_smv(symbol.arg(i)))
        if symbol.decl().name() == 'and':
          res =  '(' + ' & '.join(child) + ')'
        elif symbol.decl().name() == 'or':
          res = '(' + ' | '.join(child) + ')'
        else:
          print('error: operator \"' + symbol.decl().name() + '\" is not supported')
          sys.exit()
    return res

  def gen_var_decl(self, var):
    var_decl = ''

    for v in var:
      name = v.decl().name()
      if v.sort() == BoolSort():
        var_decl += '\t' + name + ' : boolean;\n'
#			elif v[0].sort() == IntSort():
#				var_decl += '\t' + name + ' : integer;\n'
#			elif v[0].sort() == RealSort():
#				var_decl += '\t' + name + ' : real;\n'
      else:
        print('error: type \"' +  v.sort().name() + '\" is not supported')
        sys.exit()

    return var_decl + '\tsome_err: boolean;\n\n'

  def gen_req_decl(self, req_set, req_idx):
    req_decl = ''
    
    for i in req_idx:
      req = req_set[i].req
      line = '\tr' + str(i) + ' : '

      if req[sup_dic['tmin']] == 0 and req[sup_dic['tmax']] == 0:
        if req[sup_dic['lmin']] == 0 and req[sup_dic['lmax']] == 0:
          #t=0, l=0, a=0
          if req[sup_dic['amin']] == 0 and req[sup_dic['amax']] == 0:
            line = line + 'sup_inv('
          #t=0, l=0, a<=1
          elif req[sup_dic['amin']] <= 1 and req[sup_dic['amax']] == 1:
            line = line + 'sup_inv_act('
          #t=0, l=0, a>1
          else:
            line = line + 'sup_act('
        #t=0, l!=0, a=0
        elif req[sup_dic['amin']] == 0 and req[sup_dic['amax']] == 0:
          #t=0, l<=1, a=0
          if req[sup_dic['lmin']] <= 1 and req[sup_dic['lmax']] == 1:
            line = line + 'sup_inv_loc('
          #t=0, l>1, a=0
          else:
            line = line + 'sup_loc('
        #t=0, l!=0, a!=0
        else:
          line = line + 'sup_loc_act('
      #t!=0, l=0
      elif req[sup_dic['lmin']] == 0 and req[sup_dic['lmax']] == 0:
        #t!=0, l=0, a=0
        if req[sup_dic['amin']] == 0 and req[sup_dic['amax']] == 0:
          #t<=1, l=0, a=0
          if req[sup_dic['tmin']] <= 1 and req[sup_dic['tmax']] == 1:
            line = line + 'sup_inv_trg('
          #t>1, l=0, a=0
          else:
            line = line + 'sup_trg('
        #t!=0, l=0, a!=0
        else:
          line = line + 'sup_trg_act('
      #t!=0, l!=0, a=0
      elif req[sup_dic['amin']] == 0 and req[sup_dic['amax']] == 0:
        line = line + 'sup_trg_loc('
      #t!=0, l!=0, a!=0
      else:
        line = line + 'sup('

      par = []
      for j in range(len(sup_dic)):
        if sup_rev[j] in sup_cond:
          par.append(self.conv_exp_smv(req[j]))
        else:
          # if the interval is infinite, represent it with -1
          #if req[j] >= self.ALPHA + self.BETA:
          #	par.append('-1')
          #else:
          #	par.append(str(req[j]))
          par.append(str(req[j]))
      line += ', '.join(par)
      req_decl += line + ');\n'

    return req_decl + '\n'

  def gen_init_decl(self, cond_init, req_idx):
    init_decl = 'ASSIGN\n'
    for cond in cond_init:
      if cond.decl().name() == '=':
        name = cond.arg(0).decl().name()
        if len(name) > 3:
          if name[len(name)-3:len(name)] == '__0':
            name = name[:len(name)-3]

        val = cond.arg(1)
        if val.sort() == BoolSort():
          if val.decl().name() == 'true': val = 'TRUE'
          else: val = 'FALSE'
#				elif val.sort() == IntSort():
#					val = val.as_string()
#				elif val.sort() == RealSort():
#					val = val.numerator().as_string() + '/' + val.denominator().as_string()
        else:
          print('error: type \"' + val.sort().name() + '\" is not supported')
          sys.exit()

        init_decl += '\tinit(' + name + ') := ' + val + ';\n'
      else:
        print('error: operator \"' +  cond.decl().name() + '\" is not supported for initialization')
        sys.exit()

    err_init = []
    err = []
    for i in req_idx:
      err_init.append('r' + str(i) + '.ei')
      err.append('r' + str(i) + '.e')
    init_decl += '\tinit(some_err) := ' + ' | '.join(err_init) + ';\n'
    init_decl += '\tnext(some_err) := some_err | ' + ' | '.join(err) + ';\n\n'
    return init_decl

  def gen_smv_str(self, req_set):
    smv_str = 'MODULE main\nVAR\n'
    smv_str += self.gen_var_decl(self.SYS_VAR_RAW)
    smv_str += self.gen_req_decl(req_set, list(range(len(req_set))))
    smv_str += self.gen_init_decl(self.COND_INIT, list(range(len(req_set))))
    smv_str += 'CTLSPEC AG (some_err | EX !some_err)\n'
    return smv_str

  def gen_smv_file(self, base, dest, req_set):
    shutil.copy(base, dest)
    with open(dest, 'a') as f:
      print(self.gen_smv_str(req_set), file=f)

  def gen_smv_exec(self, base, dest, cmd, req_set):
    self.gen_smv_file(base, dest, req_set)
    return subprocess.check_output(cmd + [dest])
  
  def parse_smv_trace(self, smv_out, step, trace):
    if smv_out == []:
      if step > 0:
        for var in self.SYS_VAR:
          if var[step] not in trace:
            trace[ var[step] ] = trace[ var[step-1] ]
      if trace == {}: trace = []
      return trace, step

    line = smv_out[0]
    if NUSMV_TRUE in line: return [], -1
    if NUSMV_TRACE in line:
      self.sys_var_decl(step+1)
      if step <= 0:
        return self.parse_smv_trace(smv_out[1:], step+1, trace)
      else:
        for var in self.SYS_VAR:
          if var[step] not in trace:
            trace[ var[step] ] = trace[ var[step-1] ]
        return self.parse_smv_trace(smv_out[1:], step+1, trace)
    else:
      exp = line.split()
      if len(exp) != 3: return self.parse_smv_trace(smv_out[1:], step, trace)
      elif exp[1] != '=': return self.parse_smv_trace(smv_out[1:], step, trace)
      elif exp[0] not in self.var_dic: return self.parse_smv_trace(smv_out[1:], step, trace)
      else:
        if exp[2] == 'TRUE':
          trace[ self.SYS_VAR[ self.var_dic[exp[0]] ][step] ] = simplify(And(True))
          return self.parse_smv_trace( smv_out[1:], step, trace )
        elif exp[2] == 'FALSE':
          trace[ self.SYS_VAR[ self.var_dic[exp[0]] ][step] ] = simplify(And(False))
          return self.parse_smv_trace( smv_out[1:], step, trace )
        else:
          print('error: type not supported')
          sys.exit()

  def rt_check(self, req_set):
    smv_out = self.gen_smv_exec(NUSMV_BASE, NUSMV_FILE, NUSMV_CALL, req_set)
    smv_out = smv_out.decode('utf-8')
    sigma, step = self.parse_smv_trace(smv_out.split('\n'), -1, {})
    return sigma, step

class Requirement:
  def __init__(self, sup, req, r_id, suffix = ''):
    self.sup = sup
    self.req = req
    self.id = r_id
    self.suffix = suffix
    self.st = []
    self.cnt = []
    # time point at which the last trigger phase completed
    self.tt = []
    # time point at which the last action phase started
    self.ta = []
    self.step = -1
    self.formula = []

    if len(self.req) > len(sup_dic):
      print("error: too many SUP parameters")
      print(self.req)
      sys.exit()
    elif len(self.req) < len(sup_dic):
      print("error: too few SUP parameters")
      print(self.req)
      sys.exit()

    self.tse  = []
    self.tc   = []
    self.tee  = []
    self.tmin = req[sup_dic['tmin']]
    self.tmax = req[sup_dic['tmax']]
    self.lmin = req[sup_dic['lmin']]
    self.lmax = req[sup_dic['lmax']]
    self.ase  = []
    self.ac   = []
    self.aee  = []
    self.amin = req[sup_dic['amin']]
    self.amax = req[sup_dic['amax']]

  def conv_exp(self, symbol, step):
    if type(symbol) is bool:
      return symbol
    elif symbol.num_args() == 0:
      if str(symbol.decl()) == 'True' and symbol.decl().name() == 'true':
        return True
      elif str(symbol.decl()) == 'False' and symbol.decl().name() == 'false':
        return False
      else:
        name = symbol.decl().name() + self.suffix
        if name in self.sup.var_dic:
          return self.sup.SYS_VAR[ self.sup.var_dic[name] ][step]
        else:
          return symbol
    else:
      if symbol.decl().name() == 'not':
        return Not( self.conv_exp(symbol.arg(0), step) )
      else:
        if symbol.decl().name() == 'and':
          return And( [ self.conv_exp(symbol.arg(i), step) for i in range(symbol.num_args()) ] )
        elif symbol.decl().name() == 'or':
          return Or( [ self.conv_exp(symbol.arg(i), step) for i in range(symbol.num_args()) ] )
        elif symbol.decl().name() == 'if':
          return If( 
            self.conv_exp(symbol.arg(0), step),
            self.conv_exp(symbol.arg(1), step),
            self.conv_exp(symbol.arg(2), step)
          )
        elif symbol.decl().name() == '=':
          return self.conv_exp(symbol.arg(0), step) == self.conv_exp(symbol.arg(1), step)
        else:
          print('at supreq.conv_exp()')
          print('error: operator \"' + symbol.decl().name() + '\" is not supported')
          sys.exit()

  def var_decl(self, step):
    self.sup.sys_var_decl(step)

    self.st.append( Const('st' + str(self.id) + STEP_SUFFIX + str(step), State) )
    self.cnt.append( Int('cnt' + str(self.id) + STEP_SUFFIX + str(step)) )
    self.tt.append( Int('tt' + str(self.id) + STEP_SUFFIX + str(step)) )
    self.ta.append( Int('ta' + str(self.id) + STEP_SUFFIX + str(step)) )

    self.tse.append( self.conv_exp( self.req[sup_dic['tse']], step ) )
    self.tc.append( self.conv_exp( self.req[sup_dic['tc']], step ) )
    self.tee.append( self.conv_exp( self.req[sup_dic['tee']], step ) )
    self.ase.append( self.conv_exp( self.req[sup_dic['ase']], step ) )
    self.ac.append( self.conv_exp( self.req[sup_dic['ac']], step ) )
    self.aee.append( self.conv_exp( self.req[sup_dic['aee']], step ) )

  def model(self, start, stop):
    """Bug fix"""
    if self.step >= stop:
      return
    start = self.step + 1
    """Bug fix"""
    self.step = stop

    if start == 0:
      self.var_decl(0)
      # I(q0)
      self.formula = self.formula + [[
        If(
          self.tse[0],
          If(
            Or( Not(self.tee[0]), 0 != self.tmin ),
            And(
              self.tt[0] == -1,
              self.ta[0] == -1,
              If(
                # And( self.tc[0], 0 < self.tmax ),
                0 != self.tmax,
                self.st[0] == trg,
                self.st[0] == init
              )
            ),
            # And( self.tee[0], 0 >= self.tmin )
            And(
              self.tt[0] == 0,
              If(
                Or( Not(self.ase[0]), 0 != self.lmin ),
                And(
                  self.ta[0] == -1,
                  If(
                    0 == self.lmax,
                    self.st[0] == err,
                    self.st[0] == loc
                  )
                ),
                # And( self.ase[0], 0 >= self.lmin )
                And(
                  self.ta[0] == 0,
                  If(
                    Or( Not(self.aee[0]), 0 != self.amin ),
                    If(
                      # Or( Not(self.ac[0]), 0 >= self.amax ),
                      0 == self.amax,
                      self.st[0] == err,
                      self.st[0] == act
                    ),
                    # And( self.aee[0], 0 >= self.amin )
                    self.st[0] == init
                  )
                )
              )
            )
          ),
          And( self.st[0] == init, self.tt[0] == -1, self.ta[0] == -1 )
        ),
        self.cnt[0] == 0
      ]]
      #self.formula = [[ simplify(f) for f in self.formula[0] ]]
      start += 1

    # T(qi, qi+1)
    for i in range(start-1, stop):
      self.var_decl(i+1)

      f = []
      # Previous state is init
      f = f + [ If(
        And( self.st[i] == init ),
        If(
          self.tse[i+1],
          If(
            Or( Not(self.tee[i+1]), 0 != self.tmin ),
            And(
              self.cnt[i+1] == 0, 
              self.tt[i+1] == self.tt[i],
              self.ta[i+1] == self.ta[i],
              If(
                # And( self.tc[i+1], 0 < self.tmax ),
                0 != self.tmax,
                self.st[i+1] == trg,
                self.st[i+1] == init
              )
            ),
            # And( self.tee[i+1], 0 >= self.tmin )
            And(
              self.cnt[i+1] == 0, self.tt[i+1] == i+1,
              If(
                Or( Not(self.ase[i+1]), 0 != self.lmin ),
                # Next state is loc
                And(
                  self.ta[i+1] == self.ta[i],
                  If(
                    0 == self.lmax,
                    self.st[i+1] == err,
                    self.st[i+1] == loc
                  )
                ),
                # And( self.ase[i+1], 0 >= self.lmin )
                And(
                  self.ta[i+1] == i+1,
                  If(
                    Or( Not(self.aee[i+1]), 0 != self.amin ),
                    # Next state is act
                    If(
                      # Or( Not(self.ac[i+1]), 0 >= self.amax ),
                      0 == self.amax,
                      self.st[i+1] == err,
                      self.st[i+1] == act
                    ),
                    # And( self.aee[i+1], 0 >= self.amin )
                    self.st[i+1] == init
                  )
                )
              )
            )
          ),
          # Next state is init
          And( 
            self.st[i+1] == init, 
            self.cnt[i+1] == 0, 
            self.tt[i+1] == self.tt[i], 
            self.ta[i+1] == self.ta[i]
          )
        ),
        True
      )]

      # Previous state is trg
      f = f + [ If(
        And( self.st[i] == trg ),
        If(
          Or( Not(self.tee[i+1]), self.cnt[i] + 1 < self.tmin ),
          And(
            self.tt[i+1] == self.tt[i], self.ta[i+1] == self.ta[i],
            If(
              And( 
                #self.cnt[i] + 1 < self.tmax,
                Or( self.cnt[i] + 1 < self.tmax, self.tmax == -1 ),
                self.tc[i+1] 
              ),
              # Next state is trg (continue)
              And( 
                self.st[i+1] == trg, 
                self.cnt[i+1] == self.cnt[i] + 1
              ),
              # Or( self.cnt[i] + 1 >= self.tmax, Not(self.tc[i+1]) )
              And(
                self.cnt[i+1] == 0,
                If(
                  # And( self.tse[i+1], self.tc[i+1] ),
                  self.tse[i+1],
                  # Next state is trg (aborted and restart)
                  # since st[i] == trg, tmax > 0 can be assumed
                  self.st[i+1] == trg,
                  # Next state is init (trg aborted)
                  # if tee[i+1] then self.tmin > 1,
                  # therefore trg cannot complete at this step
                  self.st[i+1] == init
                )
              )
            )
          ),
          # And( self.tee[i+1], self.cnt[i] + 1 >= self.tmin )
          And( 
            self.cnt[i+1] == 0, self.tt[i+1] == i+1,
            If(
              Or( Not(self.ase[i+1]), 0 != self.lmin ),
              # Next state is loc
              And(
                self.ta[i+1] == self.ta[i],
                If(
                  0 == self.lmax,
                  self.st[i+1] == err,
                  self.st[i+1] == loc
                )
              ),
              # And( self.ase[i+1], 0 >= self.lmin )
              And(
                self.ta[i+1] == i+1,
                If(
                  Or( Not(self.aee[i+1]), 0 != self.amin ),
                  # Next state is act
                  If(
                    # Or( Not(self.ac[i+1]), 0 >= self.amax ),
                    0 == self.amax,
                    self.st[i+1] == err,
                    self.st[i+1] == act
                  ),
                  # And( self.aee[i+1], 0 >= self.amin )
                  If(
                    # And( self.tse[i+1], self.tc[i+1], 0 != self.tmin ),
                    And( self.tse[i+1], 0 != self.tmin ),
                    # Next state is trg (act completed and trg start)
                    self.st[i+1] == trg,
                    # Next state is init (act completed)
                    self.st[i+1] == init
                  )
                )
              )
            )
          )
        ),
        True
      )]

      # Previous state is loc
      f = f + [ If(
        And( self.st[i] == loc ),
        If(
          Or( 
            Not(self.ase[i+1]),
            self.cnt[i] + 1 < self.lmin, 
            And( self.cnt[i] + 1 > self.lmax, self.lmax != -1 )
          ),
          # Next state is loc (continue)
          And(
            self.cnt[i+1] == self.cnt[i] + 1, 
            self.tt[i+1] == self.tt[i], 
            self.ta[i+1] == self.ta[i],
            If(
              # self.cnt[i] + 1 >= self.lmax,
              And( self.cnt[i] + 1 >= self.lmax, self.lmax != -1 ),
              self.st[i+1] == err,
              self.st[i+1] == loc
            )
          ),
          # And( self.ase[i+1], self.cnt[i]+1 >= self.lmin, self.cnt[i]+1 <= self.lmax )
          And(
            self.ta[i+1] == i+1,
            If(
              Or( Not(self.aee[i+1]), 0 != self.amin ),
              # Next state is act
              And(
                self.cnt[i+1] == 0, self.tt[i+1] == self.tt[i],
                If(
                  # Or( Not(self.ac[i+1]), 0 >= self.amax ),
                  0 == self.amax,
                  self.st[i+1] == err,
                  self.st[i+1] == act
                )
              ),
              # And( self.aee[i+1], 0 >= self.amin )
              If(
                self.tse[i+1],
                If(
                  Or( Not(self.tee[i+1]), 0 != self.tmin ),
                  And(
                    self.cnt[i+1] == 0, self.tt[i+1] == self.tt[i],
                    If(
                      # And( self.tc[i+1], 0 < self.tmax ),
                      0 != self.tmax,
                      # Next state is trg (act completed and trg start)
                      self.st[i+1] == trg,
                      # Next state is init (act completed and trg aborted)
                      self.st[i+1] == init
                    )
                  ),
                  # And( self.tee[i+1], 0 >= self.tmin )
                  And(
                    self.cnt[i+1] == 0, self.tt[i+1] == i+1,
                    If(
                      0 != self.lmin,
                      # Next state is loc (act completed and loc start)
                      self.st[i+1] == loc,
                      # Next state is init (act completed twice)
                      self.st[i+1] == init
                    )
                  )
                ),
                # Next state is init (act completed)
                And( 
                  self.st[i+1] == init, 
                  self.cnt[i+1] == 0, self.tt[i+1] == self.tt[i]
                )
              )
            )
          )
        ),
        True
      )]

      # Previous state is act
      f = f + [ If(
        And( self.st[i] == act ),
        If(
          Or( 
            Not(self.aee[i+1]),
            self.cnt[i] + 1 < self.amin, 
            And( self.cnt[i] + 1 > self.amax, self.amax != -1 )
          ),
          # Next state is act (continue)
          And(
            self.cnt[i+1] == self.cnt[i] + 1, 
            self.tt[i+1] == self.tt[i],
            self.ta[i+1] == self.ta[i],
            If(
              Or( 
                Not(self.ac[i+1]), 
                # self.cnt[i] + 1 >= self.amax 
                And( self.cnt[i] + 1 >= self.amax, self.amax != -1 )
              ),
              self.st[i+1] == err,
              self.st[i+1] == act
            )
          ),
          # And( aee[i+1], cnt[i] + 1 >= amin, cnt[i] + 1 <= amax )
          If(
            self.tse[i+1],
            If(
              Or( Not(self.tee[i+1]), 0 != self.tmin ),
              And(
                self.cnt[i+1] == 0, 
                self.tt[i+1] == self.tt[i],
                self.ta[i+1] == self.ta[i],
                If(
                  # And( self.tc[i+1], 0 < self.tmax ),
                  0 != self.tmax,
                  # Next state is trg (act completed and trg start)
                  self.st[i+1] == trg,
                  # Next state is init (act completed and trg aborted)
                  self.st[i+1] == init
                )
              ),
              # And( self.tee[i+1], 0 >= self.tmin )
              And(
                self.cnt[i+1] == 0, self.tt[i+1] == i+1,
                If(
                  Or( Not(self.ase[i+1]), 0 != self.lmin ),
                  # Next state is loc (act completed and loc start)
                  And(
                    self.ta[i+1] == self.ta[i],
                    If(
                      0 == self.lmax,
                      self.st[i+1] == err,
                      self.st[i+1] == loc
                    )
                  ),
                  # And( self.ase[i+1], 0 >= self.lmin )
                  And(
                    self.ta[i+1] == i+1,
                    If(
                      0 != self.amin,
                      # Next state is act (act completed and start again)
                      # If(
                      #	Not(self.ac[i+1]),
                      #	self.st[i+1] == err,
                      #	self.st[i+1] == act
                      # ),
                      self.st[i+1] == act,
                      # Next state is init (act completed twice)
                      self.st[i+1] == init
                    )
                  )
                )
              )
            ),
            # Next state is init (act completed)
            And( 
              self.st[i+1] == init, 
              self.cnt[i+1] == 0, 
              self.tt[i+1] == self.tt[i], 
              self.ta[i+1] == self.ta[i]
            )
          )
        ),
        True
      )]

      # Previous state is err
      f = f + [ If(
        And( self.st[i] == err ),
        And( 
          self.st[i+1] == self.st[i], self.cnt[i+1] == self.cnt[i], 
          self.tt[i+1] == self.tt[i], self.ta[i+1] == self.ta[i]
        ),
        True
      )]

      self.formula = self.formula + [f]

if __name__ == "__main__":
  parser = argparse.ArgumentParser(description="supreq")
  parser.add_argument("-m", "--mode", type=str, dest="mode", help="Mode: mc or repair",required=False, default="mc")
  parser.add_argument('input', default=None)
  args = parser.parse_args()
  arg = args.input
  if len(arg) > 4:
    if arg[len(arg)-3:len(arg)] == '.py':
      arg = arg[:len(arg)-3]

  if os.path.isfile(arg + '.py') == False:
    print("Error: file not found")
    sys.exit(-1)

  mod = import_module(arg)
  sup = SUPInput(mod)

  if args.mode == "repair":
    res = sup.gen_smv_exec(NUSMV_BASE, './' + arg + '.smv', NUSMV_CALL, sup.REQ_SET)
    print(res)
    sigma, step = sup.parse_smv_trace(res.decode("utf-8").split('\n'), -1, {})
    #print "step = " + str(step)
    for i in range(step+1):
      for v in sup.SYS_VAR:
        print(v[i].decl().name() + " = " + str( sigma[ v[i] ] ))
  else:
    smv_str = 'MODULE main\nVAR\n'
    smv_str += sup.gen_var_decl(sup.SYS_VAR_RAW)
    smv_str += sup.gen_req_decl(sup.REQ_SET, range(len(sup.REQ_SET)))
    smv_str += sup.gen_init_decl(sup.COND_INIT, range(len(sup.REQ_SET)))
    smv_str += 'CTLSPEC AG (some_err | EX !some_err)\n'
    print(smv_str)

    shutil.copy(NUSMV_BASE, NUSMV_FILE)
    with open(NUSMV_FILE, 'a') as f:
      print(smv_str, file=f)