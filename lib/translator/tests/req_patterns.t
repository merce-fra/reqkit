List of supported structured English requirements and their translations:
  $ cat patterns/1.req
  Input x0001 is bool
  Input x0000 is bool
  
  ID000: Globally, it is always the case that if "x0000" holds, then "x0001" holds afterwards
  $ req2something --input patterns/1.req
  from z3 import *
  ALPHA = 30
  BETA = 10
  MAX_PTRACE=20
  x0001 = Bool('x0001')
  x0000 = Bool('x0000')
  REQ_SET=[[ x0000 , x0000 , x0000 , 0, 0, 0, 0, True, True, x0001 , 1, -1 ]]
  COND_INIT = []
  $ cat patterns/2.req
  Input x0001 is bool
  Input x0000 is bool
  
  ID000: Globally, it is always the case that if "x0000" holds, then "x0001" holds for at least 2 time units
  $ req2something --input patterns/2.req
  from z3 import *
  ALPHA = 30
  BETA = 10
  MAX_PTRACE=20
  x0001 = Bool('x0001')
  x0000 = Bool('x0000')
  REQ_SET=[[ x0000 , x0000 , x0000 , 0, 0, 0, 0, x0001 , x0001 , x0001 , 20, 20 ]]
  COND_INIT = []
  $ cat patterns/3.req
  Input x0001 is bool
  Input x0000 is bool
  
  ID000: Globally, it is always the case that if "x0000" holds, then "x0001" holds afterwards for at least 2 time units
  $ req2something --input patterns/3.req
  from z3 import *
  ALPHA = 30
  BETA = 10
  MAX_PTRACE=20
  x0001 = Bool('x0001')
  x0000 = Bool('x0000')
  REQ_SET=[[ x0000 , x0000 , x0000 , 0, 0, 0, 0, True, x0001 , x0001 , 20, 20 ]]
  COND_INIT = []
  $ cat patterns/4.req
  Input x0001 is bool
  Input x0000 is bool
  
  ID000: Globally, it is always the case that if "x0000" holds, then "x0001" holds for less than 2 time units
  $ req2something --input patterns/4.req
  from z3 import *
  ALPHA = 30
  BETA = 10
  MAX_PTRACE=20
  x0001 = Bool('x0001')
  x0000 = Bool('x0000')
  REQ_SET=[[ x0000 , x0000 , x0000 , 0, 0, 0, 0, True, x0001 , Not(x0001 ), 0, 19 ]]
  COND_INIT = []
  $ cat patterns/5.req
  Input x0001 is bool
  Input x0000 is bool
  
  ID000: Globally, it is always the case that if "x0000" holds, then "x0001" holds after at most 2 time units
  $ req2something --input patterns/5.req
  from z3 import *
  ALPHA = 30
  BETA = 10
  MAX_PTRACE=20
  x0001 = Bool('x0001')
  x0000 = Bool('x0000')
  REQ_SET=[[ x0000 , x0000 , x0000 , 0, 0, 0, 0, True, Not(x0001 ), x0001 , 0, 20 ]]
  COND_INIT = []
  $ cat patterns/6.req
  Input x0001 is bool
  Input x0000 is bool
  
  ID000: Globally, it is always the case that if "x0000" holds for at least 2 time units, then "x0001" holds afterwards
  $ req2something --input patterns/6.req
  from z3 import *
  ALPHA = 30
  BETA = 10
  MAX_PTRACE=20
  x0001 = Bool('x0001')
  x0000 = Bool('x0000')
  REQ_SET=[[ x0000 , x0000 , x0000 , 20, 20, 0, 0, True, True, x0001 , 1, -1 ]]
  COND_INIT = []
  $ cat patterns/7.req
  Input x0001 is bool
  Input x0000 is bool
  
  ID000: Globally, it is always the case that if "x0000" holds for at least 2 time units, then "x0001" holds for at least 1 time units
  $ req2something --input patterns/7.req
  from z3 import *
  ALPHA = 30
  BETA = 10
  MAX_PTRACE=20
  x0001 = Bool('x0001')
  x0000 = Bool('x0000')
  REQ_SET=[[ x0000 , x0000 , x0000 , 20, 20, 0, 0, x0001 , x0001 , x0001 , 10, 10 ]]
  COND_INIT = []
  $ cat patterns/8.req
  Input x0001 is bool
  Input x0000 is bool
  
  ID000: Globally, it is always the case that if "x0000" holds for at least 2 time units, then "x0001" holds afterwards for at least 1 time units
  $ req2something --input patterns/8.req
  from z3 import *
  ALPHA = 30
  BETA = 10
  MAX_PTRACE=20
  x0001 = Bool('x0001')
  x0000 = Bool('x0000')
  REQ_SET=[[ x0000 , x0000 , x0000 , 20, 20, 0, 0, True, x0001 , x0001 , 10, 10 ]]
  COND_INIT = []
  $ cat patterns/9.req
  Input x0001 is bool
  Input x0000 is bool
  
  ID000: Globally, it is always the case that if "x0000" holds for at least 2 time units, then "x0001" holds for less than 1 time units
  $ req2something --input patterns/9.req
  from z3 import *
  ALPHA = 30
  BETA = 10
  MAX_PTRACE=20
  x0001 = Bool('x0001')
  x0000 = Bool('x0000')
  REQ_SET=[[ x0000 , x0000 , x0000 , 20, 20, 0, 0, True, True, Not(x0001 ), 0, 10 ]]
  COND_INIT = []
