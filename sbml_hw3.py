#Assignment 3
#Name- Shubham Bagi
#SBU ID - 112672171

import ply.lex as lex
import sys
import ply.yacc as yacc

class SemanticError(Exception):
    pass

class SyntaxError(Exception):
    pass
    
class Node():
    def __init__(self):
        pass
    def check(self):
        return 0
    def result(self):
        return 0
    
class StringNode(Node):
    def __init__(self, val):
        self.val = str(val).strip('(\")|(\')')
        
    def ing(self,i):
        if i<len(self.val):
            return StringNode(self.val[i])
        else:
            raise SemanticError("Semantic Error")
            
    def valcon(self,fact):
        fact = fact.check()
        if fact in self.val:
            return BolNode(True)
        else:
            return BolNode(False)
         
    def check(self):
        return self.val
        
class NumNode(Node):
    def __init__(self, v):
        if '.' in v or 'e' in v:
            self.value = float(v)
        else:
            self.value = int(v)
    def invneg(self):
        self.value = self.value * (-1)

    def exponent(self, v):
        self.value = self.value * (10**v.check())
        
    def check(self):
        return self.value
        
        
class BolNode(Node):
    def __init__(self,bol1):
        if bol1 == 'True' or  bol1 == True:
            self.value = True
        else:
            self.value = False
            
    def check(self):
        return self.value
           
class BolOperation(Node):
    def __init__(self,operation,val1,val2):
        self.val1 = val1
        self.val2 = val2
        self.operation = operation
    
    def check(self):
        val1 = self.val1.check()
        val2 = self.val2.check()
        if not (isinstance(val1, bool) and isinstance(val2, bool)):
            raise SyntaxError("SYNTAX ERROR")
        try:
            if (self.operation == 'orelse'):
                return val1 or val2
            elif self.operation== 'andalso':
                return val1 and val2
        except Exception:
            raise SemanticError("SEMANTIC ERROR")
            
class CompBolNode(Node):
    def __init__(self,bol1):
        if bol1.check() == 'True' or  bol1.check() == True:
            self.value = False
        else:
            self.value = True
      
    def check(self):
        return self.value
        
    def result(self):
        return self.value  

class OperationNode(Node):
    def __init__(self,op,val1,val2):
        self.val1 = val1
        self.val2 = val2
        self.op = op
        
    def check(self):
        val1 = self.val1.check()
        val2 = self.val2.check()
        if isinstance(val1, bool) or isinstance(val2, bool):
            raise SyntaxError("SYNTAX ERROR")
        try:
            if self.op == '+':
                return self.val1.check() + self.val2.check()
            elif self.op == '-':
                return self.val1.check() - self.val2.check()
            elif self.op == '*':
                return self.val1.check() * self.val2.check()
            elif self.op == '/':
                return self.val1.check()/self.val2.check()
            elif self.op == 'div':
                return self.val1.check()// self.val2.check()
            elif self.op == '**':
                return self.val1.check()**self.val2.check()
            elif self.op == '%':
                return self.val1.check()%self.val2.check()
            elif self.op == '**':
                return self.val1.check()**self.val2.check()
        except Exception:
            raise SemanticError("SEMANTIC ERROR")
            

class ComparisionNode(Node):
    def __init__(self,op, val1,val2):
        self.val1 = val1
        self.val2 = val2
        self.op = op
        
    def check(self):
        val1 = self.val1.check()
        val2 = self.val2.check()
        if isinstance(val1, bool) or isinstance(val2, bool):
            raise SyntaxError("SYNTAX ERROR")
        if not ((isinstance(val1, (int,float)) and isinstance(val2, (int,float))) or (isinstance(v1, str) and isinstance(v2, str))):
            raise SyntaxError("SYNTAX ERROR")
        try:
            if self.op == '<':
                return val1 < val2
            elif self.op == '>':
                return val1 > val2
            elif self.op == '<=':
                return val1 <= val2
            elif self.op == '>=':
                return val1 >= val2
            elif self.op == '==':
                return val1== val2
            elif self.op == '<>':
                return val1!= val2
        except Exception:
            raise SemanticError("SEMANTIC ERROR")
        
                 
#Lists
class ListNode(Node):
    def __init__(self,val):
        if val:
            self.val = [val]
        else:
            self.val = []
        
    def addval(self,val2):
        self.val.append(val2)
        return self
        
    def startval(self,y):
        self.val.insert(0,y)
        return self
    
    def ing(self,i):
        if i<len(self.val):
            return self.val[i]
        else:
            raise SemanticError("Semantic Error")
        
    def valcon(self,fact):
        fact = fact.check()
        if fact in self.val:
            return BolNode(True)
        else:
            return BolNode(False)
      
    def check(self):
        x = []
        for i in self.val:
            x.append(i.check())
        return x


#Tuple
class TupleNode(Node):
    def __init__(self, val):
        if val:
            self.val = tuple(val)
            
    def ing(self,i):
        if i<len(self.val):
            return self.val[i]
        else:
            raise SemanticError("SemanticError")
  
    def check(self):
        x=[]
        for i in self.val:
            x.append(i.check())
        
        return tuple(x)
        
class Result(Node):
    def _init_(self, val):
        self.val = val
        
    def check(self):
        #print('wrwr')
        return self.val.check()
      
tokens = ('NEGATION',
          'LEFT_PARENTHESIS','RIGHT_PARENTHESIS',
          'NUMBER','BOOLEAN','INTEGER','REAL',
          'STRING', 'COLON', 'COM', 'HASH','CON','In',
          'LEFTBRACKET', 'RIGHTBRACKET',
          'PLUS','MINUS','MULT','DIVIDE','POWER', 'MOD','DIV','DIVS', 'MODS','EXPONENT',
          'LESSERTHAN','GREATTHAN','LESSEQUAL','GREATEQUAL','EQUAL','NOTEQUAL',
          'AND','OR'
          )

t_LEFT_PARENTHESIS = r'\('
t_RIGHT_PARENTHESIS = r'\)'
t_LEFTBRACKET = r'\['
t_RIGHTBRACKET = r'\]'
t_PLUS = r'\+'
t_MINUS = r'-'
t_MULT = r'\*'
t_DIVIDE = r'/'
t_DIV = r'/'
t_COM = r','
t_CON = r'::'
t_COLON = r';'
t_HASH = r'\#'
t_In = r'in'
t_POWER = r'\*\*'
t_LESSERTHAN = r'<'
t_GREATTHAN = r'>'
t_LESSEQUAL = r'<='
t_GREATEQUAL = r'>='
t_EQUAL = r'=='
t_NOTEQUAL = r'<>'
t_DIVS = r'div'
t_MODS = 'mod'
t_MOD =  r'%'
t_AND = r'andalso'
t_OR = r'orelse'
t_NEGATION = r'not'
t_EXPONENT = 'e'

def t_error(t):
    raise SyntaxError("SYNTAX ERROR_token not found")
    t.lexer.skip(1) 

precedence = (
    ('left','EQUAL'),
    ('left','OR'),
    ('left','AND'),
    ('left','NEGATION'),
    ('left','NOTEQUAL','LESSERTHAN','LESSEQUAL','GREATTHAN','GREATEQUAL'),
    ('left','In','CON'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'MULT', 'DIVIDE', 'DIV', 'DIVS', 'MOD' , 'MODS'),
    ('right', 'POWER','EXPONENT'),
    ('right', 'NEGA'),
    ('left', 'LEFTBRACKET','RIGHTBRACKET'),
    ('left', 'LEFT_PARENTHESIS','RIGHT_PARENTHESIS'),
)

lex.lex(debug=0)

def t_NUMBER(t):
    r'\d*(\d\.|\.\d)\d*(e-|e)?\d*|\d+|\d*(e-|e)\d*'
    try:
        t.value = NumNode(t.value)
    except Exception:
        raise SyntaxError("SYNTAX ERROR")
    #print(t.value)
    return t

def t_STRING(t):
    r'(\"([^\\\n]|(\\.))*?\") | (\'([^\\\n]|(\\.))*?\')'
    try:
        t.value = StringNode(t.value)
    except Exception:
        raise SyntaxError("SYNTAX ERROR")
    return t 
    
def t_BOOLEAN(t):
    r'(True)|(False)'
    t.value = BolNode(t.value)
    return t
    
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
    
t_ignore = " \t"

def p_expression_binop(t):
    '''expression : expression PLUS expression
                      | expression MINUS expression
                      | expression MULT expression
                      | expression DIVIDE expression
                      | expression POWER expression
                      | expression MOD expression
                      | expression DIV expression
                      | expression DIVS expression
                      | expression MODS expression'''
    #print(t[0],t[1],t[2])
    t[0] = OperationNode(t[2], t[1], t[3])   
    
def p_expression_compare(t):
    """expression : expression LESSERTHAN expression
                  | expression LESSEQUAL expression
                  | expression GREATTHAN expression
                  | expression GREATEQUAL expression
                  | expression EQUAL expression
                  | expression NOTEQUAL expression"""
                  
    t[0] = ComparisionNode(t[2], t[1], t[3])

def p_expression_booleanop(t):
    """expression : expression OR expression
                  | expression AND expression"""
    t[0] = BolOperation(t[2], t[1], t[3])

def p_expression_neg(t):
    """expression : MINUS expression %prec NEGA"""
    t[0] = NumNode(str(-t[2].check()))

def p_expression_booleannot(t):
    """expression : NEGATION expression"""
    t[0] = CompBolNode(t[2])

def p_factor_expr(t):
    """expression : LEFT_PARENTHESIS expression RIGHT_PARENTHESIS"""
    t[0] = t[2]
 
    
def p_expr_brackets(t):
    '''expression : LEFTBRACKET expression RIGHTBRACKET'''
    t[0] = t[2]

def p_expression_contain(t):
    '''expression : expression In expression'''    
    t[0] = t[3].valcon(t[1])


def p_expression_float(t):
    """expression : expression EXPONENT expression"""
    t[1].exponent(t[3])
    t[0] = t[1]

    
def p_expression_listtop(t):
    """term : LEFTBRACKET in_list RIGHTBRACKET
           | LEFTBRACKET RIGHTBRACKET"""
    if len(t)>3:
        t[0] = t[2]
    else:
        t[0] = ListNode(None)
        
def p_expression_inlist(t):
    """in_list : in_list COM expression
               | expression"""
    if len(t)>2:
        t[1].addval(t[3])
        t[0] = t[1]
    else:
        temp = ListNode(None)
        temp.addval(t[1])
        t[0] = temp 
        
def p_list_concatenate(t):
    """expression : expression CON expression"""
    t[0] = t[3].startval(t[1])

def p_string_index(t):
    """expression : expression LEFTBRACKET expression RIGHTBRACKET"""
    if (isinstance(t[1], ListNode) or isinstance(t[1], StringNode)) and isinstance(t[3], NumNode):
        pass
    else:
        raise Exception('SEMANTIC ERROR') 
    t[0] = t[1].ing(t[3].check())
    
def p_expression_tuple(t):
    """term : LEFT_PARENTHESIS tupleinside RIGHT_PARENTHESIS
            | LEFT_PARENTHESIS RIGHT_PARENTHESIS"""
    if len(t) <= 3:
        t[0] = TupleNode(list())
    else:
        t[0] = TupleNode(t[2].val)

def p_tupleinside1(t):
    '''tupleinside : tupleinside COM expression
                   | expression'''
    if len(t) > 2:
        t[1].addval(t[3])
        t[0] = t[1]
    else:
        temp=ListNode(None)
        temp.addval(t[1])
        t[0] = temp

def p_tuple_index(t):
    '''expression : HASH expression expression'''
    try:
        t[0] = t[3].ing(t[2].check())
    except: 
        raise SemanticError("Semantic Error")
def p_exp_stmt(t):
    """statement : expression"""
    print(t)
    t[0] = Result(t[1])

def p_term_int1(t):
    '''term : INTEGER'''
    t[0] = t[1]
def p_term_real1(t):
    '''term : REAL'''
    t[0] = t[1]
def p_factor_num(t):
    'term : NUMBER'
    t[0] = t[1]
def p_factor_boolean(t):
    'term : BOOLEAN'
    t[0] = t[1]
def p_term_string(t):
    'term : STRING'
    t[0] = t[1]
def p_expression_term(t):
    'expression : term'
    t[0] = t[1]
#Error
def p_error(t):
    #print('Error')
    raise SyntaxError("SYNTAX ERROR")

if len(sys.argv) <=0  or len(sys.argv) > 2:
    print("Improper command execution")
    exit(1)
debug=False
parser=yacc.yacc(debug=debug)
lexer = lex.lex(debug=debug)
lines=None

with open(sys.argv[1], 'r') as inputfile:
    lines = inputfile.read()
lines = lines.split('\n')
for line in lines:
    try:
        res = parser.parse(line).check()                   
    except Exception as err:
        print(str(err))
    else:
        print(str(res))