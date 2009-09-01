#!/usr/bin/python

"""
REFERENCES:
1.  Prof. Allan Gottlieb's course on Compiler Construction, Spring 2008, CIMS, NYU.
    http://www.cs.nyu.edu/courses/spring08/G22.2130-001/crib-sheet.html
2.  Alfred V. Aho, Monica S. Lam, Ravi Sethi, Jeffrey D. Ullman. Compilers: Principles, Techniques, and Tools. 2nd Edition. Addison Wesley, 2006. 
"""

# TODO:
# 1. Fix bugs, check for correctness.

import copy, logging


first   = {}
follow  = {}
M       = {}


# adapted Promela grammar
S = 'spec'
terminals = (
    '',
    'NEVER',
    'LCBRACKET',
    'RCBRACKET',
    'TRACE',
    'NOTRACE',
    'TYPEDEF',
    'NAME',
    'MYTPE',
    'ASSIGNMENT',
    'COMMA',
    'SEMICOLON',
    'XR',
    'XS',
    'UNLESS',
    'HIDDEN',
    'SHOW',
    'LOCAL',
    'INIT',
    'PRIORITY',
    'BIT',
    'BOOL',
    'BYTE',
    'SHORT',
    'INT',
    'MTYPE',
    'CHAN',
    'UNSIGNED',
    'PID',
    'LSBRACKET',
    'RSBRACKET',
    'OF',
    'PROCTYPE',
    'D_PROCTYPE',
    'LNBRACKET',
    'RNBRACKET',
    'ACTIVE',
    'PROVIDED',
    'INCREMENT',
    'DECREMENT',
    'DOT',
    'NOT',
    'SEND2',
    'RECEIVE1',
    'RECEIVE2',
    'LPBRACKET',
    'RPBRACKET',
    'EVAL',
    'SUB',
    'GOTO',
    'IF',
    'FI',
    'DO',
    'OD',
    'ATOMIC',
    'D_STEP',
    'BREAK',
    'ELSE',
    'COLON',
    'PRINT',
    'PRINTF',
    'PRINTM',
    'STRING',
    'ASSERT',
    'C_CODE',
    'C_EXPR',
    'C_DECL',
    'C_TRACK',
    'C_STATE',
    'GUARD',
    'FULL',
    'EMPTY',
    'NFULL',
    'NEMPTY',
    'AND_LOGICAL',
    'OR_LOGICAL',
    'IMPLIES',
    'LEN',
    'TIMEOUT',
    'NP_',
    'ENABLED',
    'PC_VALUE',
    'AT',
    'RUN',
    'ADD',
    'MUL',
    'DIV',
    'MODULUS',
    'AND_BITWISE',
    'XOR',
    'OR_BITWISE',
    'LTEQ',
    'GTEQ',
    'EQ',
    'NEQ',
    'LSHIFT',
    'RSHIFT',
    'ONES_COMPLEMENT',
    'TRUE',
    'FALSE',
    'SKIP',
    'NUMBER'
)

nonterminals = (
    'spec',
    'spec_prime',
    'module',
    'never',
    'trace',
    'trace_prime',
    'utype',
    'mtype',
    'mtype_prime1',
    'mtype_prime2',
    'sequence',
    'sequence_prime',
    'step',
    'step_prime1',
    'step_prime2',
    'decl_lst',
    'decl_lst_prime',
    'one_decl',
    'one_decl_prime1',
    'one_decl_prime2',
    'visible',
    'init',
    'init_prime',
    'priority',
    'typename',
    'ivar',
    'ivar_prime1',
    'ivar_prime2',
    'ivar_prime3',
    'ch_init',
    'ch_init_prime',
    'proctype',
    'proctype_prime1',
    'proctype_prime2',
    'proctype_prime3',
    'proctype_prime4',
    'proctype_prime5',
    'active',
    'active_prime',
    'enabler',
    'assign',
    'assign_prime',
    'varref',
    'varref_prime1',
    'varref_prime2',
    'send',
    'send_prime',
    'receive',
    'receive_prime',
    'receive_prime_prime',
    'poll',
    'poll_prime',
    'send_args',
    'send_args_prime',
    'arg_lst',
    'arg_lst_prime',
    'recv_args',
    'recv_args_prime',
    'recv_args_prime_prime',
    'recv_arg',
    'recv_arg_prime',
    'stmt',
    'stmt_prime1',
    'stmt_prime2',
    'options',
    'options_prime',
    'expr',
    'expr_prime',
    'chanpoll',
    'any_expr',
    'any_expr_prime1',
    'any_expr_prime2',
    'any_expr_prime3',
    'any_expr_prime4',
    'binarop',
    'andor',
    'unarop',
    'uname',
    'const'
)

# NOTE: The grammar defined here is possibly not up to date with the latest
# version of the adapted Promela grammar. The parser was a little bit
# hand-tuned.
productions = {
    'spec'                    : ( ('module', 'spec_prime'), ),
    'spec_prime'              : ( ('module', 'spec_prime'), ('SEMICOLON', 'module', 'spec_prime'), ('',) ),
    'module'                  : ( ('proctype',),
                                  ('init',),
                                  ('never',),
                                  ('trace',),
                                  ('utype',),
                                  ('mtype',),
                                  ('decl_lst',)
                                ),
    'never'                   : ( ('NEVER', 'LCBRACKET', 'sequence', 'RCBRACKET'), ),
    'trace'                   : ( ('trace_prime', 'LCBRACKET', 'sequence', 'RCBRACKET'), ),
    'trace_prime'             : ( ('TRACE',), ('NOTRACE',) ),
    'utype'                   : ( ('TYPEDEF', 'NAME', 'LCBRACKET', 'decl_lst', 'RCBRACKET'), ),
    'mtype'                   : ( ('MTYPE', 'mtype_prime1', 'LCBRACKET', 'NAME', 'mtype_prime2', 'RCBRACKET'), ),
    'mtype_prime1'            : ( ('ASSIGNMENT',), ('',) ),
    'mtype_prime2'            : ( ('COMMA', 'NAME', 'mtype_prime2',), ('',) ),
    'sequence'                : ( ('step', 'sequence_prime'), ),
    'sequence_prime'          : ( ('SEMICOLON', 'step', 'sequence_prime'), ('',) ),
    'step'                    : ( ('stmt', 'step_prime1'), ('decl_lst',), ('XR', 'varref', 'step_prime2'), ('XS', 'varref', 'step_prime2') ),
    'step_prime1'             : ( ('UNLESS', 'stmt',), ('',) ),
    'step_prime2'             : ( ('COMMA', 'varref', 'step_prime2'), ('',) ),
    'decl_lst'                : ( ('one_decl', 'decl_lst_prime'), ),
    'decl_lst_prime'          : ( ('SEMICOLON', 'one_decl', 'decl_lst_prime'), ('',) ),
    'one_decl'                : ( ('one_decl_prime1', 'typename', 'ivar', 'one_decl_prime2'), ),
    'one_decl_prime1'         : ( ('visible',), ('',) ),
    'one_decl_prime2'         : ( ('COMMA', 'ivar', 'one_decl_prime2'), ('',) ),
    'visible'                 : ( ('HIDDEN',), ('SHOW',), ('LOCAL',) ),
    'init'                    : ( ('INIT', 'init_prime', 'LCBRACKET', 'sequence', 'RCBRACKET'), ),
    'init_prime'              : ( ('priority',), ('',) ),
    'priority'                : ( ('PRIORITY', 'const'), ),
    'typename'                : ( ('BIT',), ('BOOL',), ('BYTE',), ('SHORT',), ('INT',), ('MTYPE',), ('CHAN',), ('uname',), ('UNSIGNED',), ('PID',) ),
    'ivar'                    : ( ('NAME', 'ivar_prime1', 'ivar_prime2'), ),
    'ivar_prime1'             : ( ('LSBRACKET', 'const', 'RSBRACKET'), ('',) ),
    'ivar_prime2'             : ( ('ASSIGNMENT', 'ivar_prime3',), ('',) ),
    'ivar_prime3'             : ( ('any_expr',), ('ch_init',) ),
    'ch_init'                 : ( ('LSBRACKET', 'const', 'RSBRACKET', 'OF', 'LCBRACKET', 'typename', 'ch_init_prime', 'RCBRACKET'), ),
    'ch_init_prime'           : ( ('COMMA', 'typename', 'ch_init_prime'), ('',) ),
    'proctype'                : ( ('proctype_prime1', 'proctype_prime5', 'NAME', 'LNBRACKET', 'proctype_prime2', 'RNBRACKET', 'proctype_prime3', 'proctype_prime4', 'LCBRACKET', 'sequence', 'RCBRACKET'), ),
    'proctype_prime1'         : ( ('active',), ('',) ),
    'proctype_prime2'         : ( ('decl_lst',), ('',) ),
    'proctype_prime3'         : ( ('priority',), ('',) ),
    'proctype_prime4'         : ( ('enabler',), ('',) ),
    'proctype_prime5'         : ( ('PROCTYPE',), ('D_PROCTYPE',) ),
    'active'                  : ( ('ACTIVE', 'active_prime'), ),
    'active_prime'            : ( ('LSBRACKET', 'const', 'RSBRACKET'), ('',) ),
    'enabler'                 : ( ('PROVIDED', 'LNBRACKET', 'expr', 'RNBRACKET'), ),
    'assign'                  : ( ('varref', 'assign_prime'), ),
    'assign_prime'            : ( ('ASSIGNMENT', 'any_expr'), ('INCREMENT',), ('DECREMENT',) ),
    'varref'                  : ( ('NAME', 'varref_prime1', 'varref_prime2'), ),
    'varref_prime1'           : ( ('LSBRACKET', 'any_expr', 'RSBRACKET'), ('',) ),
    'varref_prime2'           : ( ('DOT', 'varref'), ('',) ),
    'send'                    : ( ('varref', 'send_prime'), ),
    'send_prime'              : ( ('NOT', 'send_args'), ('SEND2', 'send_args') ),
    'receive'                 : ( ('varref', 'receive_prime'), ),
    'receive_prime'           : ( ('RECEIVE1', 'receive_prime_prime'), ('RECEIVE2', 'receive_prime_prime') ),
    'receive_prime_prime'     : ( ('recv_args',), ('LPBRACKET', 'recv_args', 'RPBRACKET') ),
    'poll'                    : ( ('varref', 'poll_prime'), ),
    'poll_prime'              : ( ('RECEIVE1', 'LSBRACKET', 'recv_args', 'RSBRACKET'), ('RECEIVE2', 'LSBRACKET', 'recv_args', 'RSBRACKET') ),
    'send_args'               : ( ('any_expr', 'send_args_prime'), ),
    'send_args_prime'         : ( ('LNBRACKET', 'arg_lst', 'RNBRACKET'), ('arg_lst_prime',) ),
    'arg_lst'                 : ( ('any_expr', 'arg_lst_prime'), ),
    'arg_lst_prime'           : ( ('COMMA', 'any_expr', 'arg_lst_prime'), ('',) ),
    'recv_args'               : ( ('recv_arg', 'recv_args_prime'), ),
    'recv_args_prime'         : ( ('recv_args_prime_prime',), ('LNBRACKET', 'recv_args', 'RNBRACKET') ),
    'recv_args_prime_prime'   : ( ('COMMA', 'recv_arg', 'recv_args_prime_prime'), ('',) ),
    'recv_arg'                : ( ('varref',), ('EVAL', 'LNBRACKET', 'varref', 'RNBRACKET'), ('recv_arg_prime', 'const') ),
    'recv_arg_prime'          : ( ('SUB',), ('',) ),
    'stmt'                    : ( ('send',),
                                  ('receive',),
                                  ('assign',),
                                  ('GOTO', 'NAME'),
                                  ('IF', 'options', 'FI'),
                                  ('DO', 'options', 'OD'),
                                  ('ATOMIC', 'LCBRACKET', 'sequence', 'RCBRACKET'),
                                  ('D_STEP', 'LCBRACKET', 'sequence', 'RCBRACKET'),
                                  ('LCBRACKET', 'sequence', 'RCBRACKET'),
                                  ('BREAK',),
                                  ('ELSE',),
                                  ('NAME', 'COLON', 'stmt'),
                                  ('stmt_prime2', 'LNBRACKET', 'STRING', 'stmt_prime1', 'RNBRACKET'),
                                  ('PRINTM', 'LNBRACKET', 'NAME', 'RNBRACKET'),
                                  ('ASSERT', 'expr'),
                                  ('expr',),
                                  ('C_CODE', 'LCBRACKET', 'RCBRACKET'),
                                  ('C_EXPR', 'LCBRACKET', 'RCBRACKET'),
                                  ('C_DECL', 'LCBRACKET', 'RCBRACKET'),
                                  ('C_TRACK', 'LCBRACKET', 'RCBRACKET'),
                                  ('C_STATE', 'LCBRACKET', 'RCBRACKET')
                                ),
    'stmt_prime1'             : ( ('COMMA', 'arg_lst'), ('',) ),
    'stmt_prime2'             : ( ('PRINT',), ('PRINTF',) ),
    'options'                 : ( ('GUARD', 'sequence', 'options_prime'), ),
    'options_prime'           : ( ('GUARD', 'sequence', 'options_prime'), ('',) ),
    'expr'                    : ( ('any_expr', 'expr_prime'), ('LNBRACKET', 'expr', 'RNBRACKET', 'expr_prime'), ('chanpoll', 'LNBRACKET', 'varref', 'RNBRACKET', 'expr_prime') ),
    'expr_prime'              : ( ('andor', 'expr', 'expr_prime'), ('',) ),
    'chanpoll'                : ( ('FULL',), ('EMPTY',), ('NFULL',), ('NEMPTY',) ),
    'any_expr'                : ( ('LNBRACKET', 'any_expr', 'any_expr_prime4', 'RNBRACKET', 'any_expr_prime3'),
                                  ('unarop', 'any_expr', 'any_expr_prime3'),
                                  ('LEN', 'LNBRACKET', 'varref', 'RNBRACKET', 'any_expr_prime3'),
                                  ('poll', 'any_expr_prime3'),
                                  ('varref', 'any_expr_prime3'),
                                  ('const', 'any_expr_prime3'),
                                  ('TIMEOUT', 'any_expr_prime3'),
                                  ('NP_', 'any_expr_prime3'),
                                  ('ENABLED', 'LNBRACKET', 'any_expr', 'RNBRACKET', 'any_expr_prime3'),
                                  ('PC_VALUE', 'LNBRACKET', 'any_expr', 'RNBRACKET', 'any_expr_prime3'),
                                  ('NAME', 'LSBRACKET', 'any_expr', 'RSBRACKET', 'AT', 'NAME', 'any_expr_prime3'),
                                  ('RUN', 'NAME', 'LNBRACKET', 'any_expr_prime1', 'RNBRACKET', 'any_expr_prime2', 'any_expr_prime3')
                                ),
    'any_expr_prime1'         : ( ('arg_lst',), ('',) ),
    'any_expr_prime2'         : ( ('priority',), ('',) ),
    'any_expr_prime3'         : ( ('binarop', 'any_expr', 'any_expr_prime3'), ('',) ),
    'any_expr_prime4'         : ( ('IMPLIES', 'any_expr', 'COLON', 'any_expr'), ('',) ),
    'binarop'                 : ( ('ADD',), ('SUB',), ('MUL',), ('DIV',), ('MODULUS',), ('AND_BITWISE',), ('XOR',), ('OR_BITWISE',), ('LPBRACKET',), ('RPBRACKET',), ('LTEQ',), ('GTEQ',),
                                  ('EQ',), ('NEQ',), ('LSHIFT',), ('RSHIFT',), ('andor',)
                                ),
    'andor'                   : ( ('AND_LOGICAL',), ('OR_LOGICAL',) ),
    'unarop'                  : ( ('ONES_COMPLEMENT',), ('SUB',), ('NOT',) ),
    'uname'                   : ( ('NAME',), ),
    'const'                   : ( ('TRUE',), ('FALSE',), ('SKIP',), ('NUMBER',) )
}

# cheap, retrofitted hack that helps to supress spurious warnings
taboo_1_2 = {
#  ('sequence',        'SEMICOLON')  : [ ('step', 'sequence_prime')  ],
#  ('proctype_prime2', 'RNBRACKET')  : [ ('decl_lst', 'RNBRACKET')   ]
}

# cheap retrofit hack
taboo_3 = {}


def get_first():
  """Computes FIRST sets."""
  # rule 1: for all terminal t, FIRST(t) = { t }
  for t in terminals:
    first[t] = set( [t] )
    logging.debug( 'rule 1: first[' + t + '] = ' + str(list(first[t])) )
  # rule 2: for all nonterminal A, FIRST(A) = {}
  for A in nonterminals:
    first[A] = set()
    logging.debug( 'rule 2: first[' + A + '] = ' + str(list(first[A])) )
  # rule 3: if A -> '' is a production, add '' to FIRST(A)
  for A in productions:
    for production in productions[A]:
      if len(production) == 1 and production[0] == '':
        first[A] |= set( [''] )
        logging.debug( 'rule 3: first[' + A + '] = ' + str(list(first[A])) )
  while True:
    old = copy.deepcopy(first)
    # rule 4a
    # first, default, mandatory case
    for A in productions:
      for production in productions[A]:
        # first case; NOTE: used to include <and production[0] != ''> <and '' not in production> but it seems redundant and causes problems
        if len(production) >= 1:
          before, first[A] = first[A], first[A] | first[production[0]]
          if before != first[A]: logging.debug( 'rule 4a: first[' + A + '] = ' + str(list(first[A])) )
    # the thorough case; must TEST later!
    for A in productions:
      for production in productions[A]:
        if len(production) > 1:
          i = 0
          for symbol in production:
            if '' in list(first[symbol]) and i < len(production)-1:
              for x in first[ production[i+1] ]:
                # FIXME: last condition is an ad hoc hack
                if x in terminals and x != '':
                  before, first[A] = first[A], first[A] | set( [x] )
                  if before != first[A]: logging.debug( 'rule 4a: first[' + A + '] = ' + str(list(first[A])) )
            else:
              break
            i += 1
            # rule 4b: if we got this far, all symbol in production must have ''!
            before, first[A] = first[A], first[A] | set( [''] )
            if before != first[A]: logging.debug( 'rule 4b: first[' + A + '] = ' + str(list(first[A])) )
    if first == old:
      break


def get_follow():
  """Allegedly computes FOLLOW sets."""
  for A in nonterminals:
    follow[A] = set()
  # rule 6
  follow[S] = set(['$'])
  logging.debug( 'rule 6: follow[' + S + '] = ' + str(list(follow[S])) )
  while True:
    old = copy.deepcopy(follow)
    # rule 6a (heuristic, actually, let's see if it works)
    for A in productions:
      for production in productions[A]:
        #if len(production) > 1:
        for j in xrange(len(production)-1):
          if production[j] in nonterminals:
            before, follow[production[j]] = follow[production[j]], follow[production[j]] | first[production[j+1]] - set([''])
            if before != old: logging.debug( 'rule 6a: follow[' + production[j] + '] |= first[' + production[j+1] + '] = ' + str(list(follow[production[j]])) )
        # rule 6b: last, default, mandatory case
        #if len(production) > 0:
        # last case
        if production[-1] in nonterminals:
          before, follow[production[-1]] = follow[production[-1]], follow[production[-1]] | follow[A]
          if before != old: logging.debug( 'rule 6b: follow[' + production[-1] + '] |= follow[' + A + '] = ' + str(list(follow[production[-1]])) )
    for A in productions:
      for production in productions[A]:
        # the logic for beta =~> should be correct, even if it is approximate
        for j in xrange(len(production)-1, -1, -1):
          # FIXED: the entire beta should be empty, not just a component within it
          # FIXME: again, this is approximate, because beta =*> epsilon is not checked
          if '' in list(first[production[j]]):
            # FIXED: don't just pass...work!
            if j > 0 and production[j-1] in nonterminals:
              before, follow[production[j-1]] = follow[production[j-1]], follow[production[j-1]] | follow[A]
              if before != old: logging.debug( 'rule 6c: follow[' + production[j-1] + '] |= follow[' + A + '] = ' + str(list(follow[production[j-1]])) )
          else:
            if production[j] in nonterminals:
              before, follow[production[j]] = follow[production[j]], follow[production[j]] | follow[A]
              if before != old: logging.debug( 'rule 6c: follow[' + production[j] + '] |= follow[' + A + '] = ' + str(list(follow[production[j]])) )
            break
    if follow == old:
      break


def get_predictive_parsing_table():
  """Allegedly computes a predictive parsing table."""
  # TODO: incorporate taboo1_2 and taboo3 here
  for A in productions:
    for production in productions[A]:
      # Interpreting what [1, pp. 221] says about computing FIRST for any string X1, X2...XN on computing FIRST(alpha)
      production_first, allEpsilon = [], True
      production_first = production_first + list(first[production[0]])
      if len(production) > 1:
        for i in xrange(len(production)-1):
          if '' in first[production[i]]:
            production_first = production_first + [x for x in first[production[i+1]] if x != '']
          else:
            allEpsilon = False
            break
      # gotta check last one
      if '' not in first[production[-1]]:
        allEpsilon = False
      # FIXED: first[A] or first[production[0]]? dragon book or prof gottlieb? answer: prof gottlieb!
      # FIXME: Should this be a set? ANSWER: Depends on whether M is a set.
      for a in set(production_first):
        # FIXME: last condition is an ad hoc hack
        if a in terminals and a != '':
          try:
            M[(A, a)].add(production)
          except KeyError:
            M[(A, a)] = set([production])
          logging.debug( 'rule 1: M[' + A + ', ' + a + '] = ' + str(production) )
      # rule 2: putting theory to test
      if allEpsilon == True:
        for b in follow[A]:
          if b in terminals:
            try:
              M[(A, b)].add(production)
            except:
              M[(A, b)] = set([production])
            logging.debug( 'rule 2: M[' + A + ', ' + b + '] = ' + str(production) )
        if '$' in follow[A]:
          try:
            M[(A, '$')].add(production)
          except KeyError:
            M[(A, '$')] = set([production])
          logging.debug( 'rule 2: M[' + A + ', $] = ' + str(production) )
  # check for unique multiply defined entries; FIXME: Actually, M[key] shouldn't be a set,
  # if only to catch bugs (e.g. due to allEpsilon approximation) that make rule 1 and rule 2 produce the same entries, see?
  # before that, clean up taboos
  for key in taboo_1_2:
    if key != ('sequence', 'SEMICOLON'):
      for production in taboo_1_2[key]:
        M[key].remove(production)
  for key in M:
    if len(M[key]) > 1:
      logging.warn( "M has multiply defined entry! M[" + str(key) + "] = " + str(M[key]) )
    #M[key] = M[key].pop() # M[key][0]


def check_for_LL1():
  """
  Approximate piece of crap. Approximate because it doesn't ACTUALLY see whether Alpha =*> epsilon;
  this will be fixed in the future. Temporarily making use of retrofitted taboo lists.
  """
  # condition 1 & 2: FIRST(alpha) and FIRST(beta) are disjoint (pp.224, dragon book)
  # FIXME: check whether alpha and beta both actually derive the empty string
  for A in productions:
    joint = []
    for alpha in productions[A]:
      joint = joint + list(first[alpha[0]])
      if len(alpha) > 1:
        for i in xrange(len(alpha)-1):
          if '' in first[alpha[i]]:
            joint = joint + [x for x in first[alpha[i+1]] if x != '']
          else:
            break
    if len(joint) - len(set(joint)) > 0:
      for terminal in set([x for x in joint if joint.count(x) > 1]):
        try:
          if (A, terminal) not in taboo_1_2:
            print "Condition 1-2: Nonterminal " + str(A) + " has joint FIRST sets for its productions! joint = " + str( set([x for x in joint if joint.count(x) > 1]) )
        except KeyError:
          print "Condition 1-2: Nonterminal " + str(A) + " has joint FIRST sets for its productions! joint = " + str( [x for x in joint if joint.count(x) > 1] )
  # condition 3 is actually OK; probably usually needs manual resolution since LL(1) doesn't look too far ahead
  for A in productions:
    for alpha in productions[A]:
      # FIXED: of course I must place it here, not in the first FOR loop...DUH!
      joint, allEpsilon = [], True
      if len(alpha) > 1:
        for i in xrange(len(alpha)-1):
          if '' not in first[alpha[i]]:
            allEpsilon = False
            break
      # gotta check last one
      if '' not in first[alpha[-1]]:
        allEpsilon = False
      # TODO: incorporate taboo3
      if allEpsilon == True:
        for beta in [production for production in productions[A] if production != alpha]:
            joint = joint + list(first[beta[0]])
            if len(beta) > 1:
              for i in xrange(len(beta)-1):
                if '' in first[beta[i]]:
                  joint = joint + list(first[beta[i+1]])
                else:
                  break
            if set(joint) & follow[A] != set([]):
              print "Condition 3: Nonterminal " + str(A) + " has " + str(alpha) + " =?> epsilon and thus possibly a joint FIRST[" + str(beta) + "] and FOLLOW[" + str(A) + "]! joint = " + str( set(joint) & follow[A] )


def derivesEpsilon(symbol):
  """TODO later."""
  if symbol in terminals and symbol != '':
    return False
  elif symbol in nonterminals:
    # recursion
    pass
  else:
    raise Exception, "symbol '" + str(symbol) + "' not in terminals or nonterminals!"


if __name__ == '__main__':
  logging.basicConfig(level=logging.DEBUG, format='%(asctime)s %(levelname)-8s %(message)s', datefmt='%a, %d %b %Y %H:%M:%S', filename='ComputeSet.log', filemode='w')
  get_first()
  get_follow()
  get_predictive_parsing_table()
      
  print '\nLL1 CONFLICTS:\n'
  check_for_LL1()
  print '\nFIRST SETS:\n'
  for s in sorted(first):
    print s + ', ' + str(list(first[s]))
  print '\nFOLLOW SETS:\n'
  for s in sorted(follow):
    print s + ', ' + str(list(follow[s]))
  print '\nPREDICTIVE PARSING TABLE:'
  print '(nonterminal, terminal) : production,\n'
  print '{'
  for s in sorted(M):
    print str(s) + ' : ',
    entries = list(M[s])
    for n in xrange( len(entries)-1 ) :
      print str(entries[n]) + ", ",
    print str(entries[-1]),
    print
  print '}'
