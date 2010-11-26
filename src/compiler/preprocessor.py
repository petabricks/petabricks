#! /usr/bin/python

import sys, os

#####################################################
# Lexer
#####################################################

states = (
   ('oblivious','exclusive'),
   ('comment','exclusive'),
)

# Reserved Words
reserved = {
  'main' : 'MAIN',
  'memoized' : 'MEMORIZED',
  'transform' : 'TRANSFORM',
  'function' : 'FUNCTION',
  'template' : 'TEMPLATE',
  'generator' : 'GENERATOR',
  'gen' : 'GENERATOR',
  'config' : 'CONFIG',
  'param' : 'PARAM',
  'parameter' : 'PARAM',
  'tunable' : 'TUNABLE',
  'accuracy_metric' : 'ACCURACYMETRIC',
  'accuracy_bins' : 'ACCURACYBINS',
  'accuracy_variable' : 'ACCURACYVARIABLE',

  'where' : 'WHERE',
  'priority' : 'PRIORITY',
  'primary' : 'PRIMARY',
  'secondary' : 'SECONDARY',
  'rotatable' : 'ROTATABLE',
  'recursive' : 'RECURSIVE',
  'duplicate' : 'DUPLICATE',
  'rule' : 'RULE',

  'to' : 'TO',
  'from' : 'FROM',
  'using' : 'THROUGH',
  'through' : 'THROUGH',

  '#include' : 'INCLUDE',
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
tokens = ['ID', 'LBRACE', 'RBRACE', 'CLBRACE', 'CRBRACE', 'LSQBRACE', 'RSQBRACE', 'LPAREN', 'RPAREN', 'LCHEV', 'RCHEV', 'COMMA', 'LINECOMMENT', 'BLOCKCOMMENT', 'LCOMMENT', 'RCOMMENT', 'REMARK', 'FILE',  'OTHER', 'LINE'] + list(reserved.values())


# Both States
t_ANY_ignore = ' \t\r'

def t_ANY_LINECOMMENT(t):
  r'//.*'
  pass

def t_ANY_LCOMMENT(t):
  r'/\*'
  t.lexer.push_state('comment')
  pass

def t_comment_RCOMMENT(t):
  r'\*/'
  t.lexer.pop_state()
  pass

def t_comment_BLOCKCOMMENT(t):
  r'[^\n]'
  pass

def t_ANY_newline(t): 
  r'\n+'
  t.lexer.lineno += t.value.count("\n")

def t_ANY_REMARK(t):
  r'(\#[^ \n]*)'
  t.type = reserved.get(t.value,'REMARK')    # Check for reserved words
  return t

def t_ANY_error(t):
  print_error("Illegal character '%s'" % t.value[0], t.lexer.lineno)
  t.lexer.skip(1)


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
t_OTHER = r'([=.*/^+-] | [0-9] | [:?] | [!] | [&] | [|])+'

def t_FILE(t):
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
  p[5].extend(p[6])    
  p[4].extend(p[5])
  p[3].extend(p[4])
  p[1].extend(p[3]) #don't have to extend defines (define ifdef etc.)
  p[0] = p[1]

def p_petabricks_base(p):
  'petabricks : empty'
  p[0] = []

#----------------------------------------------------
# include					   
#----------------------------------------------------
def p_include_base(p):
  'include : empty'
  p[0] = []

def p_include(p):
  'include : INCLUDE FILE'	#TODO: signature
  filename = p[2]
  p[0] = parse_file_to_ast(filename[1:len(filename)-1])

#----------------------------------------------------
# define undef				   
#----------------------------------------------------
def p_define_base(p):
  'define : empty'

def p_define(p):
  'define : DEFINE ID'
  current_dict = define_dict_list[-1]
  if p[2] not in current_dict or current_dict[p[2]] == "":
    current_dict[p[2]] = ""
  else:
    print_error(p.lineno(1), "illegal to redefine " + p[2] + " to a differnt value.")

def p_define_macro(p):
  'define : DEFINE ID LPAREN id_list RPAREN expression'
  current_dict = macro_dict_list[-1]
  template = generate_template(p[4], p[6])
  template = (template[0], template[1], 'expression')
  if p[2] not in current_dict or current_dict[p[2]] != template:
    current_dict[p[2]] = template
  else:
    print_error(p.lineno(1), "illegal to redefine " + p[2] + " to a differnt value.")

def p_define_macro_block(p):
  'define : DEFINE ID LPAREN id_list RPAREN LBRACE LINE RBRACE'
  current_dict = macro_dict_list[-1]
  template = generate_template(p[4], p[7])
  template = (template[0], template[1], 'block')
  if p[2] not in current_dict or current_dict[p[2]] != template:
    current_dict[p[2]] = template
  else:
    print_error(p.lineno(1), "illegal to redefine " + p[2] + " to a differnt value.")

def p_define_const(p):
  'define : DEFINE ID expression'
  current_dict = define_dict_list[-1]
  if p[2] not in current_dict or current_dict[p[2]] == p[3]:
    current_dict[p[2]] = p[3]
  else:
    print_error(p.lineno(1), "illegal to redefine " + p[2] + " to a differnt value.")

def p_define_un(p):
  '''define : UNDEF ID
	    | UNDEF ID LPAREN id_list RPAREN
	    | UNDEF ID LPAREN id_list RPAREN expression
	    | UNDEF ID expression'''
  current_dict = define_dict_list[-1]
  if p[2] in current_dict:
    del current_dict[p[2]]

  current_dict = macro_dict_list[-1]
  if p[2] in current_dict:
    del current_dict[p[2]]

def p_id_list(p):
  'id_list : ID COMMA id_list'
  p[0] = combine_element_list(p[1],p[3])

def p_id_list_base(p):
  'id_list : ID'
  p[0] = [p[1]]

#----------------------------------------------------
# ifdef ifndef if elif				   
#----------------------------------------------------

def p_ifdefs(p):
  'ifdefs : ifdef'
  p[0] = p[1]

def p_ifdefs_base(p):
  'ifdefs : empty'
  p[0] = []

def p_ifdef(p):
  '''ifdef : IFDEF cond_id petabricks elif_block
	   | IFDEF cond_id body elif_block'''
  if p[2]: # true when define or != 0
    p[0] = p[3]
  else:
    p[0] = p[4]

def p_ifndef(p):
  '''ifdef : IFNDEF cond_id petabricks elif_block
	   | IFNDEF cond_id body elif_block'''
  if p[2]: # true when define
    p[0] = p[4]
  else:
    p[0] = p[3]

def p_if(p):
  '''ifdef : IF cond_number petabricks elif_block
	   | IF cond_number body elif_block'''
  if p[2]: # true when != 0
    p[0] = p[3]
  else:
    p[0] = p[4]

def p_elif_block(p):
  '''elif_block : ELIF cond_number petabricks elif_block
	        | ELIF cond_number body elif_block'''
  if p[2]: # true when != 0
    p[0] = p[3]
  else:
    p[0] = p[4]

def p_elif_block_else(p):
  '''elif_block : ELSE petabricks ENDIF
		| ELSE body ENDIF'''
  p[0] = p[2]

def p_elif_block_endif(p):
  'elif_block : ENDIF'
  p[0] = []

def p_cond_id(p):
  'cond_id : ID'
  p[0] = p[1] in define_dict_list[-1]

def p_cond_id_line(p):
  'cond_id : LINE'
  p[0] = p[1] in define_dict_list[-1]

def p_cond_number(p):
  'cond_number : OTHER'
  p[0] = int(p[1])

def p_cond_number_line(p):
  'cond_number : LINE'
  p[0] = int(p[1])

#----------------------------------------------------
# transform				   
#----------------------------------------------------
def p_transform_base(p):
  'transform : empty'
  p[0] = []

def p_transform(p):
  'transform : transform_headers LBRACE body RBRACE' 
  # Add { } abround body
  combine_list_element(p[3], (queue[-1],p.lineno(4),p[4]))
  combine_element_list((queue[-1],p.lineno(2),p[2]), p[3])

  # Check if it is function
  headers = p[1]
  for i in range(len(headers)):
    (filename, lineno, header_type), args = headers[i] # header = ((filename, lineno, type), args)

    if header_type == 'function':
      headers[i] = ((filename, lineno, 'transform'), args)
      ident = args[0][2]
      parsed_dict[queue[-1]].append(ident); #TODO: signature
      modify_function_ast(p)
      break
    elif header_type == 'transform':
      ident = args[0][2]
      parsed_dict[queue[-1]].append(ident); #TODO: signature
      break

  p[0] = [('transform', (p[1], p[3]))]

def p_transform_headers(p):
  'transform_headers : transform_header transform_headers'
  p[0] = combine_element_list(p[1], p[2])

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
		      | ACCURACYBINS float_list'''
  p[0] = ((queue[-1],p.lineno(1),p[1]), p[2])

def p_transform_header_element(p):
  '''transform_header : TRANSFORM ID
		      | FUNCTION ID
		      | GENERATOR ID
		      | ACCURACYMETRIC ID
		      | PARAM ID
		      | TEMPLATE chevron_arg
		      | ACCURACYVARIABLE config_arg
		      | CONFIG config_arg
		      | TUNABLE config_arg'''
  p[0] = ((queue[-1],p.lineno(1),p[1]), [(queue[-1],p.lineno(1),p[2])])

def p_transform_header_main(p):
  'transform_header : MAIN TRANSFORM ID'
  p[0] = ((queue[-1],p.lineno(1), "main transform"), [(queue[-1],p.lineno(3),p[3])])

def p_chevron_arg(p):
  'chevron_arg : LCHEV configs RCHEV'
  p[0] = p[1] + p[2] + p[3]

def p_configs_multiple(p):
  'configs : config_arg COMMA configs'
  p[0] = p[1] + ',' + p[3]

def p_configs_to(p):
  'configs : config_arg OTHER config_arg'
  p[0] = p[1] + p[2] + p[3]

def p_configs_base(p):
  'configs : config_arg'
  p[0] = p[1]

def p_config_arg_with_paren(p):
  'config_arg : ID LPAREN num_list_string RPAREN'
  p[0] = p[1] + '(' + p[3] + ')'

def p_config_arg_without_paren(p):
  '''config_arg : ID LPAREN RPAREN
                | ID
		| OTHER'''
  p[1] = replace_all_define(p[1], p.lineno(1))	# Replace defines
  p[0] = p[1]

def p_num_list_string(p):
  'num_list_string : OTHER COMMA num_list_string'
  p[0] = p[1] + ',' + p[3]

def p_num_list_string_base(p):
  'num_list_string : OTHER'
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
  p[0] = (filename, lineno, ident + '[' + p[3] + ']')

def p_matrix_without_dimension(p):
  'matrix : ident'
  p[0] = p[1]

def p_ident_common(p):
  'ident : ID'
  p[0] = (queue[-1], p.lineno(1), p[1])

def p_ident_withchevs(p):
  'ident : ID chevron_arg'
  p[0] = (queue[-1], p.lineno(1), p[1] + p[2])

def p_dimensions(p):
  'dimensions : expression COMMA dimensions'
  p[1] = replace_all_define(p[1], p.lineno(1))	# Replace defines
  p[0] = p[1] + ',' + p[3]

def p_dimensions_base(p):
  'dimensions : expression'
  p[1] = replace_all_define(p[1], p.lineno(1))	# Replace defines
  p[0] = p[1]

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
  '''expression : ID
                | OTHER'''
  p[0] = p[1]

def p_float_list(p):
  'float_list : OTHER COMMA float_list'
  p[0] = combine_element_list((queue[-1],p.lineno(1), p[1]), p[3])

def p_float_list_base(p):
  'float_list : OTHER'
  p[0] = [(queue[-1],p.lineno(1), p[1])]

def p_body_standard(p):
  'body : LINE body'
  p[1] = replace_all_define(p[1], p.lineno(1))	# Replace defines
  p[0] = combine_element_list((queue[-1],p.lineno(1),p[1]), p[2])

def p_body_ifdef(p):
  'body : ifdef body'
  p[1].extend(p[2])
  p[0] = p[1]

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
    print "Syntax error"

def p_empty(p):
  'empty :'
  pass


#####################################################
# Lexer-Parser Helper Functions
#####################################################

def print_error(message, lineno):
  print "Line", lineno, ":", message

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

""" Modify 'function' ast to 'transform' ast, and add 'from' and 'to' for body """
def modify_function_ast(p):
  #'transform : transform_headers LBRACE body RBRACE'
  #             p[1]                     [3]
  
  # Create to & from for body
  to_list = []
  from_list = []
  for header in p[1]:
    header_type, args = header # header = ((filename, lineno, type), args)

    if header_type[2] == 'to' or header_type[2] == 'through':
      to_list.extend(format_function_args(args))

    if header_type[2] == 'from':
      from_list.extend(format_function_args(args))

  first_line = "to (" + ",".join(to_list)  + ") from (" + ",".join(from_list) + ")"

  # Combind to & from to the beginning of the body
  combine_element_list((queue[-1],p.lineno(3),first_line), p[3])

  # Add { } abround body again after adding from & to
  combine_list_element(p[3], (queue[-1],p.lineno(4),p[4]))
  combine_element_list((queue[-1],p.lineno(2),p[2]), p[3])

""" Generate list of 'for' and 'to' arguments inside the body, and change 'transform' arguments
    Return  list of 'for' and 'to' arguments inside the body """
def format_function_args(args):
  body_args = []

  # Since arg = (filename, lineno, arg_name), we only need name which is the element at index 2,
  # we want only name without dimension.
  for i in range(len(args)):
    # Get name only
    (filename, lineno, arg) = args[i]

    # Remove dimension
    end = arg.find('[')
    if end != -1:
      # matrix
      arg = arg[:end]
      body_args.append(arg + " " + arg) # to(OUT OUT)
    else:
      # not matrix
      body_args.append("_" + arg + " " + arg) # need to(_OUT OUT)
      args[i] = (filename, lineno, "_" + arg)

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
  current_define = define_dict_list[-1]
  current_macro = macro_dict_list[-1]
  
  # Replace defined macros
  for macro in current_macro:
    i = s.find(macro)
    while i != -1:
      s,i = replace_macro(macro, current_macro[macro], s, i, lineno)
      i = s.find(macro,i)

  # Replace defined constants
  for define in current_define:
    i = s.find(define)
    while i != -1:
      s,i = replace_definition(define, current_define[define], s, i, lineno)
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
        break;

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
parsed_dict = {}
queue = []
define_dict_list = []
macro_dict_list = []

current_file = ""
current_line = 0

""" Return a formatted string of a line.
    Args: tuple of (full path filename, line number, content) """
def line_format((filename, lineno, content)):
  global current_file, current_line

  if current_file != filename or current_line + 10 < lineno:
    info = "\n# " + str(lineno) + " \"" + filename + "\"\n"
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
    header_type = header[0]
    args = header[1]
    output += line_format(header_type)
    
    if args != None:
      i = 0
      while i < len(args) - 1:
        arg = args[i]
        output += line_format((arg[0], arg[1], arg[2] + ','))
        i += 1
      arg = args[i]
      output += line_format(arg)
  return output

""" Convert AST of a transform into a string """
def convert_transform(tokens):
  headers = tokens[0]
  body = tokens[1]
  return convert_headers(headers) + convert_block(body)

""" Convert the entire AST into a string. """
def convert_ast_to_string(ast):
  output = ""
  for main_node in ast:
    if main_node[0] == "ccode":
      output += convert_block(main_node[1])
    elif main_node[0] == "transform":
      output += convert_transform(main_node[1])
  return output

""" Parse a content in a given file into ast. """
def parse_file_to_ast(file_path):

  if len(queue) > 0:
    current_dir = os.path.dirname(queue[-1])
  else:
    current_dir = ""

  full_path_string = os.path.abspath(os.path.join(current_dir, file_path))

  if full_path_string in parsed_dict:
    return []

  parsed_dict[full_path_string] = [] #TODO: use for transform_signature
  queue.append(full_path_string)
  define_dict_list.append({})
  macro_dict_list.append({})

  import ply.lex as lex
  import ply.yacc as yacc

  lex.lex()
  yacc.yacc()

  reader = open(full_path_string, 'r')
  input_string = reader.read()
  reader.close()
  ast = yacc.parse(input_string)

  queue.pop()
  define_dict_list.pop()
  macro_dict_list.pop()
  return ast

def main(argv=sys.argv):
  
  if len(argv) != 2:
    print "type: python test.py <input_file>"
    return

  ast = parse_file_to_ast(os.path.abspath(argv[1]))
  print convert_ast_to_string(ast)

def test_lex():
  import ply.lex as lex

  lex.lex()
  lex.input('''
#define ASSIGN(x, bin) {bins.cell(bin) += os.cell(x); a.cell(x) = bin; os.cell(x) = 0.0; }
''')
  while 1:
    tok = lex.token()
    if not tok: break
    print tok

if __name__ == "__main__":
  main()
  #test_lex()

