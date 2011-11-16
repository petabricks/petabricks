#! /usr/bin/python

import sys, os

ODIR = "scripts"
if not os.path.isdir(ODIR):
  ODIR = "."


#####################################################
# Class
#####################################################

""" Arg class """
class Arg:
  def __init__(self, filename, lineno, name, dimensions):
    self.filename = filename
    self.lineno = lineno
    self.name = name
    if dimensions == None or len(dimensions) == 0:
      self.dimensions = None
    else:
      self.dimensions = dimensions

  def get_string(self):
    if self.dimensions == None:
      return self.name
    else:
      return self.name + '[' + ",".join(self.dimensions) + ']'

  def get_string_with(self, extra):
    if self.dimensions == None:
      return self.name + extra
    else:
      return self.name + extra + '[' + ",".join(get_dimensions_with(extra)) + ']'

  def get_dimensions_with(self, extra):
    if self.dimensions == None:
      return []
    else:
      return [x + extra for x in self.dimensions]

  def clone_for_lineno(self,new_lineno):
    return Arg(self.filename, new_lineno, self.name, self.dimensions)

#####################################################
# Lexer
#####################################################

states = (
   ('oblivious','exclusive'),
   ('comment','exclusive'),

   ('define','exclusive'),
   ('defmacro','inclusive'),
   ('defws','exclusive'),
   ('defobl','exclusive'),
)

# Reserved Words
reserved = {
  'main' : 'MAIN',
  'memoized' : 'MEMORIZED',
  'transform' : 'TRANSFORM',
  'function' : 'FUNCTION',
  'or' : 'OR',
  'template' : 'TEMPLATE',
  'generator' : 'GENERATOR',
  'gen' : 'GEN',
  'config' : 'CONFIG',
  'param' : 'PARAM',
  'parameter' : 'PARAMETER',
  'tunable' : 'TUNABLE',
  'accuracy_metric' : 'ACCURACYMETRIC',
  'accuracy_bins' : 'ACCURACYBINS',
  'accuracy_variable' : 'ACCURACYVARIABLE',
  'scaled_by' : 'SCALE',

  'to' : 'TO',
  'from' : 'FROM',
  'using' : 'USING',
  'through' : 'THROUGH',

  '#include' : 'INCLUDE',
  '#library' : 'LIBRARY',
  '#ifdef' : 'IFDEF',
  '#ifndef' : 'IFNDEF',
  '#else' : 'ELSE',
  '#if' : 'IF',
  '#elif' : 'ELIF',
  '#endif' : 'ENDIF',
  '#define' : 'DEFINE',
  '#undef' : 'UNDEF',
}

# Tokens
tokens = ['ID', 'LBRACE', 'RBRACE', 'CLBRACE', 'CRBRACE', 'LSQBRACE', 'RSQBRACE', 'LPAREN', 'RPAREN', 'LCHEV', 'RCHEV', 'COMMA', 'SEMICOLON', 'LINECOMMENT', 'BLOCKCOMMENT', 'LCOMMENT', 'RCOMMENT', 'REMARK', 'STRING',  'OTHER', 'LINE'] + list(reserved.values())

# All States
t_INITIAL_oblivious_comment_define_defobl_ignore = ' \t\r'
t_defws_ignore = ''

def t_INITIAL_oblivious_comment_newline(t): 
  r'\n+'
  t.lexer.lineno += t.value.count("\n")

def t_ANY_LINECOMMENT(t):
  r'//.*'
  pass

def t_ANY_LCOMMENT(t):
  r'/\*'
  t.lexer.push_state('comment')
  pass

def t_ANY_REMARK(t):
  r'(\#[^ \n]*)'
  t.type = reserved.get(t.value,'REMARK')    # Check for reserved wordsself.
  if t.type == 'DEFINE' or t.type == 'UNDEF' :
    t.lexer.push_state('define')
  return t

def t_ANY_error(t):
  print_error("Illegal character '%s'" % t.value[0], t.lexer.lineno)
  t.lexer.skip(1)

# comment State
def t_comment_RCOMMENT(t):
  r'\*/'
  t.lexer.pop_state()
  pass

def t_comment_BLOCKCOMMENT(t):
  r'[^\n]'
  pass

# define State

def t_define_ID(t):
  r'[a-zA-Z_][a-zA-Z_0-9]*'
  t.type = reserved.get(t.value,'ID')    # Check for reserved words
  t.lexer.pop_state()
  t.lexer.push_state('defws')
  return t

def t_defws_LINE(t):
  r'[ \t\r]+'
  t.lexer.pop_state()
  t.lexer.push_state('defobl')

def t_defws_LPAREN(t):
  r'\('
  t.lexer.push_state('defmacro')
  return t

def t_defmacro_RPAREN(t):
  r'\)'
  t.lexer.pop_state()
  return t

def t_defws_defobl_newline(t): 
  r'\n+'
  t.lexer.lineno += t.value.count("\n")
  t.lexer.pop_state()

t_defobl_LINE = r'([^/\n] | [/][^/])+'

# oblivious State
t_oblivious_LINE = r'([^\{\}/\n] | [/][^/])+'

def t_oblivious_LBRACE(t):
  r'\{'
  t.lexer.level += 1
  return t

def t_oblivious_RBRACE(t):
  r'\}'
  t.lexer.level -= 1
  if (t.lexer.level == 0):
    t.lexer.pop_state()
  return t

def t_oblivious_CRBRACE(t):
  r'%\}'
  t.lexer.level -= 1
  t.lexer.pop_state()
  if (t.lexer.level == 0):
    return t
  print_error("Number of '{' does not match with number of '}'", t.lexer.lineno)
  t.lexer.skip(1)

def t_oblivious_CLBRACE(t):
  r'%\{'
  print_error("'%{' cannot be inside transform body.", t.lexer.lineno)
  t.lexer.skip(1)

# Initial State
t_RBRACE = r'\}'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LSQBRACE = r'\['
t_RSQBRACE = r'\]'
t_LCHEV = r'(<)+'
t_RCHEV = r'(>)+'
t_COMMA = r'\,'
t_SEMICOLON = r'\;'
t_OTHER = r'[0-9:?!&|=.*/^+-]+(e[0-9+-]+)?'

def t_STRING(t):
  r'\"[^\"]*\"'
  return t

def t_ID(t):
  r'[a-zA-Z_][a-zA-Z_0-9]*'
  t.type = reserved.get(t.value,'ID')    # Check for reserved words
  return t

def t_LBRACE(t):
  r'\{'
  t.lexer.level = 1
  t.lexer.push_state('oblivious')
  return t

def t_CLBRACE(t):
  r'%\{'
  t.lexer.level = 1
  t.lexer.push_state('oblivious')
  return t

#####################################################
# Parser
#####################################################

def p_petabricks(p):
  'petabricks : include define ifdefs transform ccode petabricks'    
  if not include:      
    p[5].extend(p[6])    
    p[4].extend(p[5])
    p[3].extend(p[4])
    p[1].extend(p[3]) #don't have to extend defines (define ifdef etc.)
    p[0] = p[1]
  else:
    p[0] = []
    ''' for signature
    p[4].extend(p[6])
    p[3].extend(p[4])
    p[0] = p[3]'''
    

def p_petabricks_base(p):
  'petabricks : empty'
  p[0] = []

def p_suppress_warnings(p):
  '''petabricks : LCOMMENT 
		| RCOMMENT 
		| REMARK 
		| LINECOMMENT 
		| BLOCKCOMMENT'''
  p[0] = []

#----------------------------------------------------
# include					   
#----------------------------------------------------
def p_include_base(p):
  'include : empty'
  p[0] = []

def p_include(p):
  'include : INCLUDE STRING'
  filename = p[2]
  filename = filename[1:len(filename)-1]

  if not seperate: 
    p[0] = parse_file_to_ast(filename)	# include everything
  else:
    p[0] = get_define(filename)		# include define

def p_include_library(p):
  'include : LIBRARY STRING'
  p[0] = [('library', (queue[-1],p.lineno(2),"#library " + p[2]))]

#----------------------------------------------------
# define undef				   
#----------------------------------------------------
def p_define_base(p):
  'define : empty'

def p_define(p):
  'define : DEFINE ID'
  if not define_on[-1]:
    return
  define_dict[p[2]] = ""

def p_define_macro(p):
  'define : DEFINE ID LPAREN id_list RPAREN LINE'
  if not define_on[-1]:
    return
  template = generate_template(p[4], p[6])
  template = (template[0], template[1], 'expression')
  if p[2] not in macro_dict or macro_dict[p[2]] != template:
    macro_dict[p[2]] = template
  #else:
    #print_error("illegal to redefine " + p[2], p.lineno(1))

def p_define_const(p):
  'define : DEFINE ID LINE'
  if not define_on[-1]:
    return
  define_dict[p[2]] = p[3]

def p_define_un(p):
  '''define : UNDEF ID
	    | UNDEF ID LPAREN id_list RPAREN
	    | UNDEF ID LPAREN id_list RPAREN LINE
	    | UNDEF ID LINE'''
  if not define_on[-1]:
    return
  if p[2] in define_dict:
    del define_dict[p[2]]

  if p[2] in macro_dict:
    del macro_dict[p[2]]

def p_id_list(p):
  'id_list : ID COMMA id_list'
  p[0] = combine_element_list(p[1],p[3])

def p_id_list_base(p):
  'id_list : ID'
  p[0] = [p[1]]

#----------------------------------------------------
# ifdef ifndef if elif				   
#----------------------------------------------------
neg = False

def p_ifdefs(p):
  'ifdefs : ifdef'
  p[0] = p[1]

def p_ifdefs_base(p):
  'ifdefs : empty'
  p[0] = []

def p_ifdef(p):
  '''ifdef : IFDEF cond_id petabricks elif_block
	   | IFDEF cond_id body elif_block
	   | IFDEF cond_id transform_headers elif_block'''
  if p[2]: # true when define or != 0
    p[0] = p[3]
  else:
    p[0] = p[4]

def p_ifndef(p):
  '''ifdef : ifndef cond_id petabricks elif_block
	   | ifndef cond_id body elif_block
	   | ifndef cond_id transform_headers elif_block'''
  if p[2]: # true when define
    p[0] = p[4]
  else:
    p[0] = p[3]

def p_ifndef_key(p):
  'ifndef : IFNDEF'
  p[0] = p[1]
  global neg
  neg = True

def p_if(p):
  '''ifdef : IF cond_number petabricks elif_block
	   | IF cond_number body elif_block
	   | IF cond_number transform_headers elif_block'''
  if p[2]: # true when != 0
    p[0] = p[3]
  else:
    p[0] = p[4]

def p_elif_block(p):
  '''elif_block : elif cond_number petabricks elif_block
                | elif cond_number body elif_block
                | elif cond_number transform_headers elif_block'''
  if p[2]: # true when != 0
    p[0] = p[3]
  else:
    p[0] = p[4]

def p_elif_block_else(p):
  '''elif_block : else petabricks endif
                | else body endif
                | else transform_headers endif'''
  p[0] = p[2]

def p_elif_block_endif(p):
  'elif_block : endif'
  p[0] = []

def p_cond_id(p):
  'cond_id : ID'
  p[0] = p[1] in define_dict
  append_define_on(p[0])

def p_cond_id_line(p):
  'cond_id : LINE'
  p[0] = p[1] in define_dict
  append_define_on(p[0])

def p_cond_number(p):
  'cond_number : OTHER'
  p[0] = int(p[1])
  append_define_on(p[0] != 0)

def p_cond_number_line(p):
  'cond_number : LINE'
  p[0] = int(p[1])
  append_define_on(p[0] != 0)

def p_elif(p):
  'elif : ELIF'
  p[0] = p[1]
  define_on.pop()

def p_else(p):
  'else : ELSE'
  p[0] = p[1]
  last = define_on.pop()
  append_define_on(not last)

def p_endif(p):
  'endif : ENDIF'
  p[0] = p[1]
  define_on.pop()

# helper function
def append_define_on(x):
  if not define_on[-1]:
    define_on.append(False)
    return

  global neg
  if neg:
    define_on.append(not x)
  else:
    define_on.append(x)
  neg = False

#----------------------------------------------------
# transform				   
#----------------------------------------------------
function = None
transform_name = None

def p_transform_base(p):
  'transform : empty'
  p[0] = []

def p_transform(p):
  'transform : transform_headers LBRACE body RBRACE options'
  
  if not include:
    # Add { } abround body
    combine_list_element(p[3], (queue[-1],p.lineno(4),p[4]))
    combine_element_list((queue[-1],p.lineno(2),p[2]), p[3])

    if function:
      combine_element_list(p[3],p[5])
      modify_function_ast(p)

    for i in range(len(p[1])):
      ((filename, lineno, t),args) = p[1][i]	# header = ((filename, lineno, t),args)
      if t == 'scaled_by':
        if len(args) != 1:
          print_error("can only scaled_by one transform",lineno)

        p[1].pop(i)
        modify_scale_by_ast(p, args[0].name)
        return

    p[0] = [('transform', (p[1], p[3]))]
  else:
    p[0] = []
    ''' for signature
    p[0] = [('signature', p[1])]'''

def print_headers(headers):
  for header in headers:
    (filename, lineno, t),args = header
    print t
    print [arg.name for arg in args]
    print [arg.get_string() for arg in args]

def modify_scale_by_ast(p, scale_function):
  #'transform : transform_headers LBRACE body RBRACE options'
  #		p[1]			 p[3]

  header = p[1][-1]		# header = ((filename, lineno, t),args)
  lineno = header[0][1]
  inner_headers = p[1][:]

  # Create to & from list
  to_strings = []
  from_strings = []
  through_strings = []
  from_args = []
  for header in p[1]:
    header_type, args = header 	# header = ((filename, lineno, type), args)

    if header_type[2] == 'to' or header_type[2] == 'through' or header_type[2] == 'using':
      to_strings.extend([arg.name for arg in args])

    if header_type[2] == 'from':
      from_strings.extend([arg.name for arg in args])
      through_strings.extend([arg.name + '_' for arg in args])
      from_args.extend(args)

  # add through to headers
  through_args = [Arg(queue[-1], lineno, arg.name + '_', arg.get_dimensions_with('_')) for arg in from_args]
  p[1].append(((queue[-1], lineno, 'through'),through_args))

  # add accuracy_variable to headers
  acc_vars = [n for n in arg.get_dimensions_with('_') for arg in from_strings]
  if len(acc_vars) > 0:
    acc_vars = list(set(acc_vars))
    acc_vars = [Arg(queue[-1], lineno, n, None) for n in acc_vars]
    p[1].append(((queue[-1], lineno, 'accuracy_variable'),acc_vars))
  
  # creat rule's to_list and from_list
  rule_from_list = [name + ' ' + name for name in from_strings]
  rule_to_list = [name + ' ' + name for name in to_strings] + [name + ' ' + name for name in through_strings]

  # create new body for the main transform
  first_line = "to (" + ",".join(rule_to_list)  + ") from (" + ",".join(rule_from_list) + ")"
  line = '{ ' + first_line + '{ '
  new_body = [(queue[-1],lineno,line)]

  # scale_by(in_, in);
  for name in from_strings:
    line = scale_function + '(' + name + '_, ' + name + '); '
    new_body.append( (queue[-1],lineno,line) )

  # transform_name_(out, in_);
  line = transform_name + '_(' + ",".join(to_strings + through_strings) + '); } }' 	# end rule & end transform
  new_body.append( (queue[-1],lineno,line) )

  #TODO: inner transform
  lineno = p.lineno(2)
  for i in range(len(inner_headers)):
    ((old_filename, old_lineno, t), args) = inner_headers[i] 	# header = ((filename, lineno, type), args)
    inner_headers[i] = ((queue[-1], lineno, t), [arg.clone_for_lineno(lineno) for arg in args])
    if t == 'transform':
      args = inner_headers[i][1]
      arg = args[0]
      arg.name = arg.name + '_'

  p[0] = [('transform', (p[1], new_body)), ('transform', (inner_headers, p[3]))]

def p_transform_signature(p):
  'transform : transform_headers SEMICOLON' 

  p[0] = [('signature', p[1])]

def p_options(p):
  'options : OR LBRACE body RBRACE options'
  if not function:
    print_error("\'or\' is not allowed after \'transform\'", p.lineno(1))

  # Add { } abround body
  combine_list_element(p[3], (queue[-1],p.lineno(4),p[4]))
  combine_element_list((queue[-1],p.lineno(2),p[2]), p[3])
  p[0] = combine_element_list(p[3],p[5])

def p_options_base(p):
  'options : empty'
  p[0] = []

def p_transform_headers(p):
  'transform_headers : transform_header transform_headers'
  p[0] = combine_element_list(p[1], p[2])

def p_transform_headers_ifdef(p):
  'transform_headers : ifdef transform_headers'
  p[1].extend(p[2])
  p[0] = p[1]

def p_transform_headers_base(p):
  'transform_headers : empty'
  p[0] = []

def p_transform_header_noarg(p):
  'transform_header : MEMORIZED'
  p[0] = ((queue[-1],p.lineno(1),p[1]), None)

def p_transform_header_list(p):
  '''transform_header : FROM matrix_list
		      | TO matrix_list
		      | THROUGH matrix_list
		      | USING matrix_list
		      | ACCURACYBINS float_list'''
  p[0] = ((queue[-1],p.lineno(1),p[1]), p[2])

def p_transform_header_list_macro(p):
  'transform_header : ACCURACYBINS ID'
  p[2] = replace_all_define(p[2], p.lineno(2))	# Replace defines
  p[0] = ((queue[-1],p.lineno(1),p[1]), [Arg(queue[-1], p.lineno(2), p[2], None)])

def p_transform_header_element(p):
  '''transform_header : TRANSFORM ID
		      | FUNCTION ID
		      | GEN ID
		      | GENERATOR ID
		      | ACCURACYMETRIC ID
		      | PARAM ID
		      | PARAMETER ID
		      | TEMPLATE template_arg
		      | ACCURACYVARIABLE config_arg
		      | CONFIG config_arg
		      | TUNABLE config_arg
		      | SCALE ID'''
  global function, transform_name
  if p[1] == "transform":
    function = False
    transform_name = p[2]
  elif p[1] == "function":
    function = True
    p[1] = "transform"
    transform_name = p[2]

  p[2] = replace_all_define(p[2], p.lineno(2))	# Replace defines
  p[0] = ((queue[-1],p.lineno(1),p[1]), [Arg(queue[-1], p.lineno(2), p[2], None)])

def p_transform_header_main(p):
  'transform_header : MAIN TRANSFORM ID'
  p[0] = ((queue[-1],p.lineno(1), "main transform"), [Arg(queue[-1], p.lineno(3), p[3], None)])


def p_configs_multiple(p):
  'configs : config_arg COMMA configs'
  p[0] = p[1] + ',' + p[3]

'''def p_configs_to(p):
  'configs : config_arg OTHER config_arg'
  p[0] = p[1] + p[2] + p[3]'''

def p_configs_base(p):
  'configs : config_arg'
  p[0] = p[1]

def p_config_arg_with_paren(p):
  #TODO: sure?
  'config_arg : config_flags ID LPAREN num_list_string RPAREN'
  p[0] = p[1] + ' ' + p[2] + '(' + p[4] + ')'

def p_config_arg_without_paren(p):
  #TODO: sure?
  '''config_arg : config_flags ID LPAREN RPAREN
                | config_flags ID
                | config_flags OTHER'''
  p[0] = p[1] + ' ' + p[2]

def p_config_flags(p):
  #TODO: sure?
  'config_flags : config_flags ID'
  p[0] = p[1] + ' ' + p[2]

def p_config_flags_param(p):
  #TODO: sure?
  'config_flags : config_flags ID LPAREN num_list_string RPAREN'
  p[0] = p[1] + ' ' + p[2] + '(' + p[4] + ')'

def p_config_flags_end(p):
  'config_flags : '
  p[0] = ''

def p_num_list_string(p):
  'num_list_string : OTHER COMMA num_list_string'
  p[0] = p[1] + ',' + p[3]

def p_num_list_string_base(p):
  '''num_list_string : OTHER
                     | ID'''
  p[0] = p[1]

def p_matrix_list(p):
  'matrix_list : matrix COMMA matrix_list'
  p[0] = combine_element_list(p[1], p[3])

def p_matrix_list_base(p):
  'matrix_list : matrix'
  p[0] = [p[1]]

def p_matrix_with_dimensions(p):
  '''matrix : ident LSQBRACE dimensions RSQBRACE'''
  (filename, lineno, ident) = p[1]
  p[0] = Arg(filename, lineno, ident, p[3])

def p_matrix_without_dimension(p):
  'matrix : ident'
  (filename, lineno, ident) = p[1]
  p[0] = Arg(queue[-1], lineno, ident, None)

def p_ident_common(p):
  'ident : ID'
  p[1] = replace_all_define(p[1], p.lineno(1))	# Replace defines
  p[0] = (queue[-1], p.lineno(1), p[1])

def p_ident_withchevs(p):
  'ident : ID chevron_arg'
  p[1] = replace_all_define(p[1], p.lineno(1))	# Replace defines
  p[2] = replace_all_define(p[2], p.lineno(2))	# Replace defines
  p[0] = (queue[-1], p.lineno(1), p[1] + p[2])

def p_chevron_arg(p):
  '''chevron_arg : LCHEV chevron_exp RCHEV'''
  p[0] = p[1] + p[2] + p[3]

def p_chevron_exp(p):
  '''chevron_exp : chevron_exp OTHER chevron_exp
		 | LPAREN chevron_exp RPAREN'''
  p[0] = p[1] + p[2] + p[3]

def p_chevron_exp_base(p):
  '''chevron_exp : base'''
  p[0] = p[1]

def p_chevron_exp_base_empty(p):
  '''chevron_exp : '''
  p[0] = ''

def p_base(p):
  '''base : ID
	  | OTHER
	  | STRING'''
  p[0] = p[1]

def p_template_arg(p):
  'template_arg : LCHEV configs RCHEV'
  p[0] = p[1] + p[2] + p[3]

def p_dimensions(p):
  'dimensions : expression COMMA dimensions'
  p[1] = replace_all_define(p[1], p.lineno(1))	# Replace defines
  p[0] = combine_element_list(p[1], p[3])

def p_dimensions_base(p):
  'dimensions : expression'
  p[1] = replace_all_define(p[1], p.lineno(1))	# Replace defines
  p[0] = [p[1]]

def p_expression_unary(p):
  '''expression : ID expression
                | OTHER expression'''
  p[0] = p[1] + p[2]

def p_expression_binary(p):
  '''expression : expression OTHER expression
                | expression COMMA expression
                | expression LCHEV expression
                | expression RCHEV expression
		| LPAREN expression RPAREN'''
  p[0] = p[1] + p[2] + p[3]

def p_expression_more(p):
  '''expression : expression OTHER'''
  p[0] = p[1] + p[2]

def p_expression_base(p):
  '''expression : base'''
  p[0] = p[1]

def p_float_list(p):
  'float_list : OTHER COMMA float_list'
  p[0] = combine_element_list(Arg(queue[-1], p.lineno(1), p[1], None), p[3])

def p_float_list_base(p):
  'float_list : OTHER'
  p[0] = [Arg(queue[-1], p.lineno(1), p[1], None)]

def p_body_standard(p):
  'body : LINE body'
  p[1] = replace_all_define(p[1], p.lineno(1))	# Replace defines
  p[0] = combine_element_list((queue[-1],p.lineno(1),p[1]), p[2])

def p_body_ifdef(p):
  'body : ifdef body'
  p[1].extend(p[2])
  p[0] = p[1]

def p_body_include(p):
  'body : INCLUDE body'
  p[0] = combine_element_list((queue[-1],p.lineno(1),p[1]), p[2])

def p_body_block(p):
  'body : LBRACE body RBRACE body'
  p[0] = combine_block_list((queue[-1],p.lineno(1),p[1]), p[2] , (queue[-1],p.lineno(3),p[3]), p[4])

def p_body_base(p):
  'body : empty'
  p[0] = []

#----------------------------------------------------
# embeded C code				   
#----------------------------------------------------
def p_ccode_base(p):
  'ccode : empty'
  p[0] = []

def p_ccode(p):
  'ccode : CLBRACE body CRBRACE'
  combine_list_element(p[2], (queue[-1],p.lineno(3),p[3]))
  combine_element_list((queue[-1],p.lineno(1),p[1]), p[2])
  p[0] = [('ccode',p[2])]

def p_error(p):
  if (p != None):
    print_error("Syntax error at '%s'" % p.value, p.lineno)
  else:
    print_error("Syntax error", 0)

def p_empty(p):
  'empty :'
  pass


#####################################################
# Lexer-Parser Helper Functions
#####################################################

def print_error(message, lineno):
  #print >> sys.stderr, "Line", lineno, ":", message
  #sys.exit(1)
  sys.exit(relpath(queue[-1]) + ", Line " + str(lineno) + ": " + message)

""" Insert ele at the end of l
    Return l """
def combine_list_element(l, ele):
  l.append(ele)
  return l

""" Insert ele in the fron of 
    Return l """
def combine_element_list(ele, l):
  l.insert(0,ele)
  return l

""" Return list of lines and brackets """
def combine_block_list(lbrace,l1,rbrace,l2):
  l1.insert(0,lbrace)
  l1.append(rbrace)
  l1.extend(l2)
  return l1

""" Modify 'function' ast to 'transform' ast, and add 'from' and 'to' for each rule """
def modify_function_ast(p):
  #'transform : transform_headers LBRACE body RBRACE rule_list'
  #             p[1]                        	     p[5]
  
  # Create to & from for body
  to_list = []
  from_list = []
  for header in p[1]:
    header_type, args = header # header = ((filename, lineno, type), args)

    if header_type[2] == 'to' or header_type[2] == 'through' or header_type[2] == 'using':
      #TODO: use the commented line instead, once fix the parser
      #to_list.extend([arg.name + ' ' + arg.name for arg in args])
      to_list.extend(format_function_args(args))

    if header_type[2] == 'from':
      #TODO: use the commented line instead, once fix the parser
      #from_list.extend([arg.name + ' ' + arg.name for arg in args])
      from_list.extend(format_function_args(args))

  first_line = "to (" + ",".join(to_list)  + ") from (" + ",".join(from_list) + ")"
  combine = []

  for rule in p[5]:
    lineno = rule[0][1]

    # Add to & from to the beginning of every rule
    combine.append( (queue[-1],lineno,first_line[:]) )
    combine.extend(rule)
    #combine_element_list((queue[-1],lineno,first_line[:]), body)

  # Add { } abround body again after adding from & to
  combine_list_element(combine, (queue[-1],p.lineno(4),p[4]))
  combine_element_list((queue[-1],p.lineno(2),p[2]), combine)
  p[3] = combine

""" Generate list of 'for' and 'to' arguments inside the body, and change 'transform' arguments
    Return  list of 'for' and 'to' arguments inside the body """
def format_function_args(args):
  body_args = []

  # Get name from args
  for i in range(len(args)):
    arg = args[i]
    if arg.dimensions != None:
      # matrix
      body_args.append(arg.name + " " + arg.name)
    else:
      # not matrix
      body_args.append("_" + arg.name + " " + arg.name) # need to(_OUT OUT)
      args[i].name = "_" + args[i].name

  return body_args

""" Return an index after word if word starting at string[index] is an isolated word (not preceeding or following by a-z A-Z 0-9 _)
    Return -1 otherwise """
def skip_isolated_word(string, index, word):
  next = index + len(word)
  if (index == 0 or is_not_id(string[index-1])) and (next == len(string) or is_not_id(string[next])):
    return next
  else:
    return -1

""" Return true if x is not a-z A-Z 0-9 _ """
def is_not_id(x):
  if x >= "a" and x <= "z":
    return False
  if x >= "A" and x <= "Z":
    return False
  if x >= "0" and x <= "9":
    return False
  return x != "_"

""" Return a template for a given macro """
def generate_template(args, expression):
  for i in range(len(args)):
    arg = args[i]
    # Find the beginning ot the argument.
    index = expression.find(arg)
    while index != -1 and index + len(arg) <= len(expression):
      # If its neighbors are not a-z A-Z 0-9 _, then it's an argument.
      next = skip_isolated_word(expression, index, arg)
      if next != -1:
        expression = expression[:index] + "$" + str(i) + expression[next:]
        index += len("$" + str(i))
      else:
        index += len(arg)
      index = expression.find(arg, index)
  return (len(args), expression)

""" Replace all defined constants & macros in string s
    Return the resulted string """
def replace_all_define(s, lineno):
  # Replace defined macros
  for macro in macro_dict:
    i = s.find(macro)
    while i != -1:
      s,i = replace_macro(macro, macro_dict[macro], s, i, lineno)
      i = s.find(macro,i)

  # Replace defined constants
  for define in define_dict:
    i = s.find(define)
    while i != -1:
      s,i = replace_definition(define, define_dict[define], s, i, lineno)
      i = s.find(define,i)


  return s

""" Replace the definition at the given index in string s with expanded code
    Return (the resulted string, index after definition) """
def replace_definition(define, extended_def, s, head_index, lineno):
  next = skip_isolated_word(s, head_index, define)
  if next != -1:
    s = s[:head_index] + extended_def + s[next:]
    return (s,next)
  else:
    return (s,head_index+len(define))

""" Replace the macro at the given index in string s with expanded code
    Return (the resulted string, index after macro) """
def replace_macro(macro, (no_args, expanded, t), s, head_index, lineno):
  next = skip_isolated_word(s, head_index, macro)

  # If macro is not isolated word, return.
  if next == -1:
    return (s,head_index+len(macro))

  # If there is no '(' after macor, return.
  if next >= len(s) or s[next] != '(':
    return (s,next)

  index = next + 1 #skip '('

  # Replace macro arguments with the actual names
  for i in range(no_args):
    paren_count = 0
    start_index = index
    while index < len(s):
      # End of the agrument
      if paren_count == 0 and (s[index] == ',' or s[index] == ')'):
        break

      if s[index] == '(':
        paren_count += 1
      elif s[index] == ')':
        paren_count -= 1

      index += 1

    if index == len(s):
      print_error("too few arguments for " + macro, lineno)
    # Add actual argument name to the list
    actual_arg = s[start_index:index]
    index += 1

    # Replace arg with actual_arg
    expanded = expanded.replace("$"+str(i), actual_arg)
  #if index > 0 and index < len(s):
  if s[index-1] != ')':
    print_error("too many arguments for " + macro, lineno)

  if t == 'block':
    expanded = expanded.strip(' ;')

  return (s[0:head_index] + expanded + s[index:], index)


#####################################################
# .pbcc to AST to output string
#####################################################

# Global variable
parsed_set = set()
queue = []
define_dict = {}
macro_dict = {}
define_on = [True]

current_file = ""
current_line = 0
seperate = False
include = False

def relpath(filename):
  current = os.path.abspath("") + '/'
  common = os.path.commonprefix([current, filename])
  current = current[len(common):]

  count = current.count('/')
  out = ""
  for i in range(count):
    out += '../'
  return out + filename

""" Return a formatted string of a line.
    Args: tuple of (full path filename, line number, content) """
def line_format((filename, lineno, content)):
  global current_file, current_line

  if current_file != filename or current_line + 10 < lineno:
    info = "\n# " + str(lineno) + " \"" + relpath(filename) + "\"\n"
    current_file = filename
    current_line = lineno
  else:
    info = ""

  newlines = ""
  while current_line < lineno:
    newlines += '\n'
    current_line += 1

  return info + newlines + content + " "

""" Convert AST of a block into a string """
def convert_block(block):
  output = ""
  for line in block:
    output += line_format(line)
  return output

""" Convert AST of headers (transform, from, to etc.) into a string """
def convert_headers(headers):
  output = ""
  for header in headers:
    header_type, args = header
    output += line_format(header_type)
    
    if args != None:
      i = 0
      while i < len(args) - 1:
        arg = args[i]
        output += line_format((arg.filename, arg.lineno, arg.get_string() + ','))
        i += 1
      if len(args) == 0:
        print "zero", header
      arg = args[i]
      output += line_format((arg.filename, arg.lineno, arg.get_string()))
  return output

""" Convert AST of a transform into a string """
def convert_transform(tokens):
  headers = tokens[0]
  body = tokens[1]
  return convert_headers(headers) + convert_block(body)

""" Convert AST of a signature into a string """
def convert_signature(headers):
  return convert_headers(headers) + ';'

""" Convert the entire AST into a string. """
def convert_ast_to_string(ast):
  output = ""
  for main_node in ast:
    if main_node[0] == "ccode":
      output += convert_block(main_node[1])
    elif main_node[0] == "transform":
      output += convert_transform(main_node[1])
    elif main_node[0] == "signature":
      output += convert_signature(main_node[1])
    elif main_node[0] == "library":
      output += line_format(main_node[1])
  return output

def update_cleanup():
  queue.pop()

""" Parse a content in a given file into ast. """
def parse_file_to_ast(file_path):

  if len(queue) > 0:
    current_dir = os.path.dirname(queue[-1])
  else:
    current_dir = ""

  full_path_string = os.path.abspath(os.path.join(current_dir, file_path))

  if full_path_string in parsed_set:
    return []

  parsed_set.add(full_path_string)
  queue.append(full_path_string)

  import ply.lex as lex
  import ply.yacc as yacc

  lex.lex(nowarn=1)
  yacc.yacc(debug=False, tabmodule="_preprocessor", outputdir=ODIR)

  reader = open(full_path_string, 'r')
  input_string = reader.read()
  reader.close()
  ast = yacc.parse(input_string)

  update_cleanup()
  return ast

def get_define(file_path):
  current_dir = os.path.dirname(queue[-1])
  full_path_string = os.path.abspath(os.path.join(current_dir, file_path))
  queue.append(full_path_string)

  import ply.lex as lex
  import ply.yacc as yacc

  lex.lex(nowarn=1)
  yacc.yacc(debug=False, tabmodule="_preprocessor", outputdir=ODIR)

  reader = open(full_path_string, 'r')
  input_string = reader.read()
  reader.close()

  global include
  include = True
  ast = yacc.parse(input_string)
  include = False

  update_cleanup()
  return ast


def main(argv=sys.argv):
  
  if len(argv) < 2 or len(argv) > 3:
    print "Usage: ./preprocessor.py <input_file>.pbcc [-s]"
    return

  if argv[1] == "--help" or (len(argv) == 3 and argv[2] != "-s"):
    print "Usage: ./preprocessor.py <input_file>.pbcc [-s]"
    print "\t-s\tSeperate compilation"
    return
    
  global seperate
  seperate = (len(argv) == 3)
  ast = parse_file_to_ast(os.path.abspath(argv[1]))
  print convert_ast_to_string(ast)

def test_lex():
  import ply.lex as lex

  lex.lex()
  lex.input('''
#define A
#define POISSON2D_BINS 1,3,5,7,9
#define SUM(out, x, y) { out = x + y; }
#define Y 10
''')
  while 1:
    tok = lex.token()
    if not tok: break
    print tok

if __name__ == "__main__":
  main()
  #test_lex()

