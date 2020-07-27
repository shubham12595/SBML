import ply.lex as lex
import ply.yacc as yacc
import sys as sys

class SemanticError(Exception):
    pass

class SyntaxError(Exception):
    pass

# List of token names.
tokens = (
    'NUMBER',
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE',
    'EXPONENT', 'CONCAT',
    'LPAREN', 'RPAREN',
    'LBRACKET', 'RBRACKET',
    'COMMA', 'TUPLE_INDEX',
    'INTDIV', 'MOD', 'IN', 'NOT',
    'SEMICOLON',
    'EQUAL', 'NOTEQUAL', 'GT', 'GE', 'LT', 'LE',
    'ANDALSO', 'ORELSE',
    'STRING', 'BOOLEAN'
)

# Reserved words
reserved = {
    'and' : 'AND',
    'or'  : 'OR',
    'True': 'TRUE',
    'False': 'FALSE',
    'not' : 'NOT',
    'in'  : 'IN',
    'orelse': 'ORELSE',
    'andalso': 'ANDALSO',
    'mod': 'MOD',
    'div': 'DIV'
}

# Regular expression rules for simple tokens
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_EXPONENT = r'\\'
t_CONCAT = r'\:\:'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_COMMA = r'\,'
t_TUPLE_INDEX = r'\#'
t_INTDIV = r'div'
t_MOD = r'mod'
t_IN = r'in'
t_NOT = r'not'
t_SEMICOLON = r'\;'
t_EQUAL = r'=='
t_NOTEQUAL = r'\<\>'
t_GT = r'\>'
t_GE = r'\>\='
t_LT = r'\<'
t_LE = r'\<\='
t_ANDALSO = r'andalso'
t_ORELSE = r'orelse'



# A regular expression rule with some action code
def t_NUMBER(t):
    r'\-?\d*\.\d*[Ee]\-?\d+ | \-?\d*\.\d* | -?\d+'
    #print(t.value)
    if '.' in t.value:
        t.value = float(t.value)
    else:
        t.value = int(t.value)
    return t


def t_BOOLEAN(t):
    r'True|False'
    if t.value == 'True':
        t.value = True
    else:
        t.value = False
    return t

def t_STRING(t):
    r'(\')[^(\')](\') | (\")[^(\")](\")'
    t.value = t.value[1:-1]
    return t


def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


t_ignore = ' \t'


# Error handling rule
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


lexer = lex.lex()

precedence = (
    ('left', 'ORELSE'),
    ('left', 'ANDALSO'),
    ('left', 'NOT'),
    ('left', 'LT'), ('left', 'LE'), ('left', 'EQUAL'), ('left', 'NOTEQUAL'), ('left', 'GE'), ('left', 'GT'),
    ('right', 'CONCAT'),
    ('left', 'IN'),
    ('left', 'PLUS'), ('left', 'MINUS'),
    ('left', 'TIMES'), ('left', 'DIVIDE'), ('left', 'INTDIV'), ('left', 'MOD'),
    ('right', 'EXPONENT'),
    ('left', 'LBRACKET', 'RBRACKET'),
    ('left', 'TUPLE_INDEX'),
    ('left', 'LPAREN', 'RPAREN')
)



def p_expression_exponentiation(p):
    'expression : expression EXPONENT expression'
    if type(p[1]) != type(1) and type(p[1]) != type(1.0):
        raise SemanticError()
    if type(p[3]) != type(1) and type(p[3]) != type(1.0):
        raise SemanticError()
    p[0] = p[1] ** p[2]

def p_term_multiplication(p):
    'term : expression TIMES expression'
    if type(p[1]) != type(1) and type(p[1]) != type(1.0):
        raise SemanticError()
    if type(p[3]) != type(1) and type(p[3]) != type(1.0):
        raise SemanticError()
    p[0] = p[1] * p[3]

def p_term_div(p):
    'term : expression DIVIDE expression'
    if type(p[1]) != type(1) and type(p[1]) != type(1.0):
        raise SemanticError()
    if type(p[3]) != type(1) and type(p[3]) != type(1.0):
        raise SemanticError()
    if p[3] == 0:
        raise SemanticError()
    p[0] = p[1] / p[3]

def p_expression_integer_division(p):
    'expression : expression INTDIV expression'
    if type(p[1]) != type(1) and type(p[1]) != type(1.0):
        raise SemanticError()
    if type(p[3]) != type(1) and type(p[3]) != type(1.0):
        raise SemanticError()
    p[0] = p[1] // p[3]

def p_expression_modulus(p):
    'expression : expression MOD expression'
    if type(p[1]) != type(1) and type(p[1]) != type(1.0):
        raise SemanticError()
    if type(p[3]) != type(1) and type(p[3]) != type(1.0):
        raise SemanticError()
    p[0] = p[1] % p[3]

def p_expression_addition(p):
    'expression : expression PLUS expression'
    if type(p[1]) == type(1) or type(p[1]) == type(1.0):
        if type(p[3]) != type(1) and type(p[3]) != type(1.0):
            raise SemanticError()
        else:
            pass
    elif type(p[1]) != type("") or type(p[1]) != type([]):
        if type(p[1]) != type(p[3]):
            raise SemanticError()
    p[0] = p[1] + p[3]

def p_expression_subtraction(p):
    'expression : expression MINUS expression'
    if type(p[1]) != type(1) and type(p[1]) != type(1.0):
        raise SemanticError()
    if type(p[3]) != type(1) and type(p[3]) != type(1.0):
        raise SemanticError()
    p[0] = p[1] - p[3]

def p_expression_membership(p):
    'expression : expression IN expression'
    if type(p[3]) != type("") and type(p[3]) != type([]):
        raise SemanticError()
    p[0] = p[1] in p[3]

def p_expression_concatenation(p):
    'expression : expression CONCAT expression'
    if type(p[3]) != type([]):
        raise SemanticError()
    p[3].insert(0, p[1])
    p[0] = p[3]

def p_expression_negation(p):
    'expression : NOT expression'
    if type(p[2]) != type(True):
        raise SemanticError()
    p[0] = not p[2]

def p_expression_conjunction(p):
    'expression : expression ANDALSO expression'
    if type(p[1]) != type(True) or type(p[3]) != type(True):
        raise SemanticError()
    p[0] = p[1] and p[3]

def p_expression_disjunction(p):
    'expression : expression ORELSE expression'
    if type(p[1]) != type(True) or type(p[3]) != type(True):
        raise SemanticError()
    p[0] = p[1] or p[3]

def p_expression_lessthan(p):
    'expression : expression LT expression'
    if type(p[1]) == type(1) or type(p[1]) == type(1.0):
        if type(p[3]) != type(1) and type(p[3]) != type(1.0):
            raise SemanticError()
        else:
            pass
    if type(p[1]) == type("") or type(p[3]) == type(""):
        if type(p[3]) != type(p[1]):
            raise SemanticError()
    p[0] = p[1] < p[3]

def p_expression_lessthanorequalto(p):
    'expression : expression LE expression'
    if type(p[1]) == type(1) or type(p[1]) == type(1.0):
        if type(p[3]) != type(1) and type(p[3]) != type(1.0):
            raise SemanticError()
        else:
            pass
    if type(p[1]) == type("") or type(p[3]) == type(""):
        if type(p[3]) != type(p[1]):
            raise SemanticError()
    p[0] = p[1] <= p[3]

def p_expression_equal(p):
    'expression : expression EQUAL expression'
    if type(p[1]) == type(1) or type(p[1]) == type(1.0):
        if type(p[3]) != type(1) and type(p[3]) != type(1.0):
            raise SemanticError()
        else:
            pass
    if type(p[1]) == type("") or type(p[3]) == type(""):
        if type(p[3]) != type(p[1]):
            raise SemanticError()
    p[0] = p[1] == p[3]

def p_expression_NOTEQUAL(p):
    'expression : expression NOTEQUAL expression'
    if type(p[1]) == type(1) or type(p[1]) == type(1.0):
        if type(p[3]) != type(1) and type(p[3]) != type(1.0):
            raise SemanticError()
        else:
            pass
    if type(p[1]) == type("") or type(p[3]) == type(""):
        if type(p[3]) != type(p[1]):
            raise SemanticError()
    p[0] = p[1] != p[3]

def p_expression_greaterthanorequalto(p):
    'expression : expression GE expression'
    if type(p[1]) == type(1) or type(p[1]) == type(1.0):
        if type(p[3]) != type(1) and type(p[3]) != type(1.0):
            raise SemanticError()
        else:
            pass
    if type(p[1]) == type("") or type(p[3]) == type(""):
        if type(p[3]) != type(p[1]):
            raise SemanticError()
    p[0] = p[1] >= p[3]

def p_expression_greaterthan(p):
    'expression : expression GT expression'
    if type(p[1]) == type(1) or type(p[1]) == type(1.0):
        if type(p[3]) != type(1) and type(p[3]) != type(1.0):
            raise SemanticError()
        else:
            pass
    if type(p[1]) == type("") or type(p[3]) == type(""):
        if type(p[3]) != type(p[1]):
            raise SemanticError()
    p[0] = p[1] > p[3]

def p_factor_expr(p):
    'factor : LPAREN expression RPAREN'
    p[0] = p[2]

def p_expression_list(p):
    'expression : LBRACKET list_inside RBRACKET'
    print(p[2])
    p[0] = p[2]

def p_listinside(p):
    'list_inside : expression COMMA list_inside'
    p[0] = [p[1]] + p[3]

def p_listinisde2(p):
    '''list_inside : expression'''
    p[0] = [p[1]]

def p_listinisde3(p):
    '''list_inside : '''
    p[0] = []

def p_list_index(p):
    'expression : expression LBRACKET expression RBRACKET'

    if type(p[1]) == type("") or type(p[1]) == type([]):
        pass
    else:
        raise SemanticError()
    if (p[3]) >= len(p[1]):
       raise SemanticError()

    p[0] = p[1][p[3]]


def p_expression_tuple(p):
    'expression : LPAREN tupleinside RPAREN'
    p[0] = tuple(p[2])

def p_tupleinside1(p):
    '''tupleinside : expression COMMA tupleinside'''
    p[0] = [p[1]] + p[3]

def p_tupleinside2(p):
    'tupleinside : expression'
    p[0] = [p[1]]

def p_tupleinside3(p):
    'tupleinside : '
    p[0] = []

def p_tuple_index(p):
    'expression : TUPLE_INDEX expression LPAREN expression RPAREN'
    if type(p[4]) == type((1,)):
        pass
    else:
        raise SemanticError()
    if (p[2]-1) >= len(p[4]):
        raise SemanticError()

    p[0] = p[4][p[2]-1]

def p_expression_term(p):
    'expression : term'
    p[0] = p[1]

def p_term_factor(p):
    'term : factor'
    p[0] = p[1]

def p_factor_num(p):
    'factor : NUMBER'
    p[0] = p[1]

def p_factor_boolean(p):
    'factor : BOOLEAN'
    p[0] = p[1]

def p_term_string(p):
    'term : STRING'
    p[0] = p[1]

def p_error(p):
    raise SyntaxError

parser = yacc.yacc()


# take input file as an argument
try:
    file = open(sys.argv[1], 'r')
except:
    print("ERROR: Please provide the correct input file as a argument on terminal to run the program correctly.")
    print("e.g)> python sbml.py <input_file_path>")
    exit()
try:
    for line in file:
        try:
            #print(line)
            result = parser.parse(line)
        except SemanticError:
            print("SEMANTIC ERROR")
        except SyntaxError:
            print("SYNTAX ERROR")
        else:
            print(result)
finally:
    file.close()