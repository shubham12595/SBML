#Assignment 4
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
        self.value = 0
    def check(self):
        return self.value
    
class StringNode(Node):
    def __init__(self, v):
        self.value = str(v).strip('(\")|(\')')
        #print(self.value)
        
    def ing(self,i):
        if i>=0 and i<len(self.value):
            return StringNode(self.value[i])
        else:
            raise SemanticError("Semantic Error")
            
    def valcon(self,fact):
        if isinstance(fact,Node):
            fact = fact.check()
        if fact in self.value:
            return BolNode(True)
        else:
            return BolNode(False)
         
    def check(self):
        return self.value
        
class Stack(Node):
    def __init__(self):
        self.stack = [{}]
    def push(self,val):
        self.stack.append(val)
    def pop(self):
        return self.stack.pop()
    def peep(self,index = -1):
        return self.stack[index]

stack = Stack()
        
class VarNode(Node):
    def __init__(self, vval):
        self.value = vval  
    
    def check(self):
        return stack.peep()[self.value]
    
class PrintNode(Node):
    def __init__(self, val):
        super().__init__()
        self.value = val

    def check(self):
        print(self.value.check())

                           
class IfNode(Node):
    def __init__(self, cond,val,val2= None):
        #print(cond)
        self.cond = cond
        self.val = val
        self.val2 = val2

    def check(self):
        cond = self.cond.check()
        #print(val.check())
        if cond:
            self.val.check()
        else:
            if self.val2 is not None:
                self.val2.check()
                
    def elseb(self, x):
        self.val2 = x 
                
class BlockNode(Node):
    def __init__(self,v1):
        self.v1 = v1
    
    def check(self):
        for s in self.v1:
            s.check()
            
class WhileNode(Node):
    def __init__(self, cond, val):
        #print('Enter')
        self.condition = cond
        self.val = val
        
    def check(self):
        condition = self.condition.check()
        while self.condition.check():
            self.val.check()
  
class NumNode(Node):
    def __init__(self, val):
        if '.' in val or 'e' in val:
            self.value = float(val)
        else:
            self.value = int(val)
    def invneg(self):
        self.value = self.value * (-1)

    def exponent(self, val):
        self.value = self.value * (10**val.check())
        
    def check(self):
        return self.value
        
        
class BolNode(Node):
    def __init__(self,bol1):
        if bol1 == 'True' or  bol1 == True:
            self.value = True
        else:
            self.value = False
            
    def check(self):
        #print(self.value)
        return self.value
           
class BolOperation(BolNode):
    def __init__(self,operation,val1,val2):
        self.val1 = val1
        self.val2 = val2
        self.operation = operation
    
    def check(self):
        val1 = self.val1.check()
        val2 = self.val2.check()
        #print(val1,val2)
        if not (isinstance(val1, bool) and isinstance(val2, bool)):
            raise SyntaxError("SYNTAX ERROR 1")
        try:
            if self.operation == 'orelse':
                self.value= (val1 or val2)
            elif self.operation== 'andalso':
                self.value= (val1 and val2)
            return self.value
        except Exception:
            raise SemanticError("SEMANTIC ERROR")
            
class PresentNode(Node):
    def __init__(self,v1, v2):
        self.v1 = v1
        self.v2 = v2
        
    def check(self):
        
        v1 = self.v1.check()
        v2 = self.v2.check()
        if isinstance(v2, (str, list)):
            ans = v1 in v2
            return ans
        else:
            raise SyntaxError("SYNTAX ERROR 2") 

class OperationNode(Node):
    def __init__(self,op,val1,val2):
        self.val1 = val1
        self.value = 0
        self.val2 = val2
        self.op = op
        
    def check(self):
        val1 = self.val1.check()
        val2 = self.val2.check()
        if isinstance(val1, bool) or isinstance(val2, bool):
            raise SyntaxError("SYNTAX ERROR 3")
        try:
            if self.op == '+':
                self.value= val1 + val2
            elif self.op == '-':
                self.value= val1 - val2
            elif self.op == '*':
                self.value= val1 * val2
            elif self.op == '/':
                self.value= val1/val2
            elif self.op == 'div':
                self.value= val1// val2
            elif self.op == '**':
                self.value= val1**val2
            elif self.op == '%':
                self.value= val1%val2
            elif self.op == 'mod':
                self.value= val1%val2
            elif self.op == '**':
                self.value= val1**val2
            return self.value
        except Exception:
            raise SemanticError("SEMANTIC ERROR")
            
class ComparisionNode(BolNode):
    def __init__(self,op, val1,val2):
        self.val1 = val1
        self.val2 = val2
        self.op = op
        #print(op)
        #print(val1,val2)
        
    def check(self):
        val1 = self.val1.check()
        val2 = self.val2.check()
        #print(val1,val2)
        if isinstance(val1, bool) or isinstance(val2, bool):
            raise SyntaxError("SYNTAX ERROR 4")
        if not ((isinstance(val1, (int,float)) and isinstance(val2, (int,float))) or (isinstance(v1, str) and isinstance(v2, str))):
            raise SyntaxError("SYNTAX ERROR 5")
        try:
            if self.op == '<':
                self.value=(val1 < val2)
            elif self.op == '>':
                self.value =(val1 > val2)
            elif self.op == '<=':
                self.value= (val1 <= val2)
            elif self.op == '>=':
                self.value=(val1 >= val2)
            elif self.op == '==':
                self.value=(val1== val2)
            elif self.op == '<>':
                self.value=(val1< val2 or val1>val2) 
            return self.value
        except Exception:
            raise SemanticError("SEMANTIC ERROR")
            
class VarInd(Node):
    def __init__(self,id,value,i = None):
        super().__init__()
        self.id = id.value
        self.value = value
        #print(self.value)
        self.i =i
        
    def check(self):
        i = self.i
        value = self.value
        id = self.id
        if i is not None:
            i = i.check()
        if i is None:
            stack.peep()[id] = value.check()
        else:
            if isinstance(id,Node):
                id.check()[i] = value.check()
            else:
                stack.peep()[id][i] = value.check()
 
class ConcatNode(Node):
    def __init__(self,val1,val2):
        self.val1 = val1
        self.val2 = val2
    def check(self):
        return [self.val1.check()] + self.val2.check()
    
class Getval(Node):
    def __init__(self, val, i):
        self.value=val
        self.i=i
        
    def check(self):
        i = self.i.check()
        if isinstance(self.value,Node):
            return self.value.check()[i]

class FunctionNode(Node):
    def __init__(self,id,args,block,ret_exp):
        self.id = id
        self.args = args
        self.block = block
        self.ret_exp = ret_exp
    
    def check(self):
        stack.peep()[self.id.value] = self
    
    def run(self,argv):
        dict = {}
        for i in range(len(argv)):
            dict[self.args.ing(i).value] = argv[i].check()
        stack.push(dict)
        self.block.check()
        x = self.ret_exp.check()
        dict = stack.pop()
        return x
 
 
class ProgramNode(Node):
    def __init__(self, block, funcs=None):
        super().__init__()
        self.block = block
        self.funcs = funcs
    def check(self):
        if self.funcs is not None:
            self.funcs.check()
        self.block.check()

class FuncCallNode(Node):
    def __init__(self,id,args):
        self.id = id
        self.args = args
    
    def check(self):
        return stack.peep(0)[self.id.value].run(self.args.value)
        
               
class NegNode(Node):
    def __init__(self,v1):
        self.v1 = v1
        
    def check(self):
        v1 = self.v1.check()
        if not isinstance(v1, bool):
            raise SyntaxError("SYNTAX ERROR 6")
        return not v1
        
#Lists
class ListNode(Node):
    def __init__(self,val = None):
        if val:
            self.value = [val]
        else:
            self.value = []
        
    def addval(self,val2):
        self.value.append(val2)
        return self
    
    def ing(self,i):
        if i>=0 and i<len(self.value):
            return self.value[i]
        else:
            raise SemanticError("Semantic Error")
        
    def valcon(self,fact):
        fact = fact.check()
        if fact in self.value:
            return BolNode(True)
        else:
            return BolNode(False)
    
    def startval(self,y):
        self.value.insert(0,y)
        return self
        
    def check(self):
        x = []
        for i in self.value:
            x.append(i.check())
        return x
        
#Tuple
class TupleNode(Node):
    def __init__(self, val):
        self.value = tuple(val)
            
    def ing(self,i):
        if i >0 and i<len(self.value):
            return self.value[i-1]
        else:
            raise SemanticError("SemanticError")
    
    def check(self):
        tup=[]
        for i in self.value:
            tup.append(i.check())
        
        return tuple(tup)
        
reserved_dict = {
    'if': 'IF',
    'else': 'ELSE',
    'while': 'WHILE',
    'print': 'PRINT',
    'not' : 'NOT',
    'mod': 'MOD',
    'div' : 'DIV',
    'andalso' : 'ANDALSO',
    'orelse': 'ORELSE',
    'in' : 'IN',
    'fun' : 'FUN',
}       
      
tokens = [
          'LEFT_PARENTHESIS','RIGHT_PARENTHESIS',
          'NUMBER','BOOLEAN','INTEGER','VAR',
          'STRING', 'COLON', 'COM', 'HASH','CON',
          'LEFTBRACKET', 'RIGHTBRACKET',
          'PLUS','MINUS','MULT','DIVIDE','POWER',
          'LESSERTHAN','GREATTHAN','LESSEQUAL','GREATEQUAL','EQUAL','NOTEQUAL',
          'EQ','LCURLY', 'RCURLY'
          ] + list(reserved_dict.values())
 
t_LEFT_PARENTHESIS = r'\('
t_RIGHT_PARENTHESIS = r'\)'
t_LEFTBRACKET = r'\['
t_RIGHTBRACKET = r'\]'
t_PLUS = r'\+'
t_MINUS = r'-'
t_MULT = r'\*'
t_DIVIDE = r'/'
t_COM = r','
t_CON = r'::'
t_COLON = r';'
t_HASH = r'[#]'
t_POWER = r'\*\*'
t_LESSERTHAN = r'<'
t_GREATTHAN = r'>'
t_LESSEQUAL = r'<='
t_GREATEQUAL = r'>='
t_EQUAL = r'=='
t_NOTEQUAL = r'<>'
t_EQ = r'\='
t_LCURLY = r'\{'
t_RCURLY = r'\}'

def t_error(t):
    #raise SyntaxError("SYNTAX ERROR_token not found")
    t.lexer.skip(1) 

precedence = (
    ('left','ORELSE'),
    ('left','ANDALSO'),
    ('left','NOT'),
    ('left','NOTEQUAL','LESSERTHAN','LESSEQUAL','GREATTHAN','GREATEQUAL','EQUAL'),
    ('left','IN','CON'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'MULT', 'DIVIDE', 'DIV', 'MOD'),
    ('right', 'POWER'),
    ('right', 'NEGA'),
    ('left', 'LEFTBRACKET','RIGHTBRACKET'),
    ('left', 'LEFT_PARENTHESIS','RIGHT_PARENTHESIS'),
)

def t_NUMBER(t):
    r'\d*(\d\.|\.\d)\d*([eE]-?\d+)?|\d+'
    #print(t.value)
    try:
        t.value = NumNode(t.value)
        
    except Exception:
        raise SyntaxError("SYNTAX ERROR 7")
    return t

def t_VAR(t):
    r'[a-zA-Z][a-zA-Z_0-9]*'
    #print(t.type)
    if t.value in reserved_dict:
        t.type = reserved_dict[t.value]
    else:
        #print(t.value)
        t.value = VarNode(t.value)
    #print(t.value)
    return t
    
def t_STRING(t):
    r'(\"([^\\\n]|(\\.))*?\") | (\'([^\\\n]|(\\.))*?\')'
    try:
        t.value = StringNode(t.value)
    except Exception:
        raise SyntaxError("SYNTAX ERROR 8")
    return t 
    
def t_BOOLEAN(t):
    r'(True)|(False)'
    t.value = BolNode(t.value)
    #print(t.value)
    return t
    
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
    
t_ignore = " \t"

def p_program_block(t):
    '''
    program : block
    '''
    #print('gfg')
    t[0] = ProgramNode(t[1])

def p_program(t):
    '''
    program : funcs block
    '''
    #print("p_program")
    #print(p[2],p[1])
    t[0] = ProgramNode(t[2], t[1])
    
    
def p_program_funcs(t):
    '''
    funcs : funcs func
          | func
    '''
    if len(t) > 2:
        t[1].addval(t[3])
        t[0] = t[1]
    else:
        t[0] = ListNode(t[1])
        
def p_function(t):
    '''
    func : FUN VAR LEFT_PARENTHESIS listel RIGHT_PARENTHESIS EQ block expression COLON
    '''
    #print('dgdg')
    t[0] = FunctionNode(t[2], t[4], t[7], t[8])
    

def p_elements(t):
    '''listel : listel COM VAR
                | VAR'''
    if len(t) > 2:
        t[1].addval(t[3])
        t[0] = t[1]
    else:
        t[0] = ListNode(t[1])

def p_block(t):
    '''block : LCURLY block RCURLY'''
    #print('curly')
    t[0] = t[2]

def p_stat(t):
    '''statement : block'''
    t[0] = t[1]

def p_statone(t):
    '''statement : stlist COLON'''
    t[0] = t[1]

def p_statsix(t):
    '''statement : while_stat'''
    t[0] = t[1]
    
def p_statfour(t):
    '''statement : if_else_stat'''
    t[0] = t[1]
def p_statfive(t):
    '''statement : if_stat'''
    t[0] = t[1]
    
def p_stattwo(t):
    '''statement : print COLON'''
    t[0] = t[1]

def p_statthree(t):
    '''statement : expression COLON'''
    t[0] = t[1]
def p_block_stlst(t):
    '''block : LCURLY statementlist RCURLY
             | LCURLY RCURLY'''
    if len(t)>3:
        t[0] = BlockNode(t[2])
    else:
        BlockNode([])

def p_stlist(t):
    '''statementlist : statementlist statement
              | statement'''
    if len(t)>2:
        t[0]=t[1] +[t[2]]
    else:
        t[0] = [t[1]]

def p_while(t):
    '''while_stat : WHILE LEFT_PARENTHESIS expression RIGHT_PARENTHESIS block'''
    #print('dgd')
    t[0] = WhileNode(t[3], t[5])

def p_stat_ifelse(t):
    '''if_else_stat : if_stat ELSE block'''
    t[1].elseb(t[3])
    t[0] = t[1]
    
def p_stat_ifcond(t):
    '''if_stat : IF LEFT_PARENTHESIS expression RIGHT_PARENTHESIS block'''
    t[0] = IfNode(t[3],t[5])

def p_var_fun(t):
    '''stlist : VAR EQ expression
              | expression LEFTBRACKET expression RIGHTBRACKET EQ expression '''
    #print(t[3])
    if len(t) == 4:
        t[0] = VarInd(t[1], t[3])
    else:
        t[0] = VarInd(t[1], t[6], t[3])
    #print(t[0])
    
def p_st_print(t):
    '''print : PRINT LEFT_PARENTHESIS expression RIGHT_PARENTHESIS'''
    #print(t[3].value)
    t[0] = PrintNode(t[3])

def p_expression_func_call(t):
    '''
    expression : VAR LEFT_PARENTHESIS listel RIGHT_PARENTHESIS
    '''
    t[0] = FuncCallNode(t[1],t[3])
    
def p_factor_expr(t):
    """expression : LEFT_PARENTHESIS expression RIGHT_PARENTHESIS"""
    t[0] = t[2]

def p_expression_binop(t):
    '''expression : expression PLUS expression
                      | expression MINUS expression
                      | expression MULT expression
                      | expression DIVIDE expression
                      | expression POWER expression
                      | expression MOD expression
                      | expression DIV expression'''
    #print(t[1].value,t[2].value)
    t[0] = OperationNode(t[2], t[1], t[3])
    #print(t[0].value)
    
def p_exp_list(t):
    '''listel : listel COM expression
                | expression'''
    if len(t) > 2:
        t[1].addval(t[3])
        t[0] = t[1]
    else:
        t[0] = ListNode(t[1])  

def p_expression_cons(t):
    '''expression : expression CON expression'''
    t[0] = ConcatNode(t[1], t[3])

def p_exp_tuple(t):
    '''tupexp : LEFT_PARENTHESIS listel RIGHT_PARENTHESIS
             | LEFT_PARENTHESIS RIGHT_PARENTHESIS'''
    if len(t) > 3:
        t[0] = TupleNode(t[2].value) 
    else:
        t[0] = TupleNode([])

def p_expre_list(t):
    '''list : LEFTBRACKET listel RIGHTBRACKET
            | LEFTBRACKET RIGHTBRACKET'''
    if len(t) > 3:
        t[0] = t[2]
    else:
        t[0] = ListNode()
      
def p_exp_tup_(t):
    '''getv : HASH expression LEFT_PARENTHESIS expression RIGHT_PARENTHESIS
                | HASH expression expression ''' 
    if len(t) > 4:
        t[0] = Getval(t[4], t[2])
    else:
        t[0] = Getval(t[3], t[2])
    
def p_expression_list_index(t):
    '''getv : expression LEFTBRACKET expression RIGHTBRACKET'''
    t[0] = Getval(t[1], t[3])
    
def p_expression_booleanop(t):
    """expression : expression ORELSE expression
                  | expression ANDALSO expression"""
    #print(t[1].value,t[3].value)
    t[0] = BolOperation(t[2], t[1], t[3])
def p_expression_neg(t):
    """expression : MINUS expression %prec NEGA"""
    t[0] = NumNode(str(-t[2].check()))
    
def p_expression_booleannot(t):
    """expression : NOT expression"""
    #print('Not Exp')
    t[0] = NegNode(t[2])

def p_expression_in(t):
    '''expression : expression IN expression'''
    #print('In Exp')
    #print(t[1].value,t[3].value)
    t[0] = PresentNode(t[1], t[3])
    #print(t[0].value)
        
def p_expression_compare(t):
    """expression : expression LESSERTHAN expression
                  | expression LESSEQUAL expression
                  | expression GREATTHAN expression
                  | expression GREATEQUAL expression
                  | expression EQUAL expression
                  | expression NOTEQUAL expression"""  
    #print(t[1].value)
    #print(t[3].value)
    t[0] = ComparisionNode(t[2], t[1], t[3])
    #print(t[0].value)
     
def p_expression_factor(t):
    '''expression : term
                  | getv'''
    t[0] = t[1]
    
def p_term_int1(t):
    '''term : INTEGER'''
    t[0] = t[1]
def p_term_string(t):
    '''term : STRING'''
    t[0] = t[1]
def p_term_var(t):
    '''term : VAR'''
    t[0] = t[1]
def p_term_list(t):
    '''term : list'''
    t[0] = t[1]
def p_term_real1(t):
    '''term : tupexp'''
    t[0] = t[1]
def p_factor_num(t):
    '''term : NUMBER'''
    t[0] = t[1]
def p_factor_boolean(t):
    '''term : BOOLEAN'''
    t[0] = t[1]

#Error
def p_error(t):
    #print('Error')
    print(t)
    #raise SyntaxError("SYNTAX ERROR 9")

if len(sys.argv) <=0  or len(sys.argv) > 2:
    print("Improper command execution")
    exit(0)
debug=False
parser=yacc.yacc(debug=debug)
lexer = lex.lex(debug=debug)
parsing=None
with open(sys.argv[1], 'r') as inputfile:
    parsing = inputfile.read() 
x = parser.parse(parsing, debug=0).check()

