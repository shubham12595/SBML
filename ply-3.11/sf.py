import sys
import ply.lex as lex
import ply.yacc as yacc

id_table={}
reserved_keywords={'div':'Div','mod':'Mod','in':'In','andalso':'Andalso','orelse':'Orelse','not':'Not','if':'If','else':'Else','while':'While','print':'Print'}

class Node():
    def _init_(self):
        self.parent = None

    def parentCount(self):
        count = 0
        current = self.parent
        while current is not None:
            count += 1
            current = current.parent
        return count


class NumberNode(Node):
    def _init_(self, val):
        if 'e' in val or '.' in val:
            self.val = float(val)
        else:
            self.val = int(val)

    def eval(self):
        return self.val
    

class BooleanNode(Node):
    def _init_(self, val, isFalse=False):
        self.val=True if val=='True' or val==True else False
        self.isFalse=isFalse

    def eval(self):
        if isinstance(self.val,Node):
            if self.isFalse:
                return not self.val.eval()
            else:
                return self.val.eval()
        else:
            if self.isFalse:
                return not self.val
            else:
                return self.val


class ListNode(Node):
    def _init_(self, val): 
        self.val=[val] if val else []

    def add(self,elem):
        self.val.append(elem)
        return self

    def iscontains(self,elem):
        return BooleanNode(True) if elem in self.val else BooleanNode(False)

    def get(self,index):
        if index>=0 and index<len(self.val):
            return self.val[index]
        else:
            raise Exception('Semantic error')

    def pappend(self,elem):
        self.val.insert(0,elem)
        return self

    def eval(self):
        op=[]
        for k in self.val:
            if isinstance(k,Node):
                op.append(k.eval())
            else:
                op.append(k)
        return op


class TupleNode(Node):
    def _init_(self, val):
        self.val=tuple(val)

    def eval(self):
        op=[]
        for k in self.val:
            if isinstance(k,Node):
                op.append(k.eval())
            else:
                op.append(k)
        return tuple(op)
    
    def get(self,index):
        if index>0 and index<=len(self.val):
            return self.val[index-1]
        else:
            raise Exception('Semantic error')



class StringNode(Node):
    def _init_(self, val):
        self.val=str(val)

    def get(self, index):
        if index < 0 or index > len(self.val):
            raise Exception('Semantic Error')
        return StringNode(self.val[index])

    def iscontains(self,elem):
        if isinstance(elem,Node):
            elem=elem.eval()
        return BooleanNode(True) if elem in self.val else BooleanNode(False)

    def eval(self):
        return self.val



class Logical(BooleanNode):
    def _init_(self,operator,lval,rval):
        super()._init_(False)
        self.operator = operator
        self.lval = lval
        self.rval = rval

    def eval(self):
        if isinstance(self.lval,Node):
            lval=self.lval.eval()
        if isinstance(self.rval,Node):
            rval=self.rval.eval()
        operator=self.operator
        if (lval=='True' or lval=='False' or lval==True or lval==False) and (rval=='True' or rval=='False' or rval==True or rval==False):
            pass
        else:
            raise Exception('Semantic error')
        try:
            if operator=='andalso':
                self.val=lval and rval
                return lval and rval
            elif operator=='orelse':
                self.val=lval or rval
                return lval or rval
            else:
                raise Exception('Syntax error')
        except:
            raise Exception('Semantic error')




class Comparision(BooleanNode):
    def _init_(self, operator,lval,rval):
        super()._init_(False)
        self.operator = operator
        self.lval = lval
        self.rval = rval

    def eval(self):
        if isinstance(self.lval,Node):
            lval=self.lval.eval()
        if isinstance(self.rval,Node):
            rval=self.rval.eval()
        operator=self.operator
        if isinstance(lval,(int,float)) and isinstance(lval,(int,float)):
            pass
        elif isinstance(lval,str) and isinstance(rval,str):
            pass
        else:
            raise Exception('Semantic error')
        
        try:
            if operator == '>=':
                self.val=lval >= rval
                return lval >= rval
            elif operator == '==':
                self.val=lval == rval
                return lval == rval
            elif operator == '<=':
                self.val=lval <= rval
                return lval <= rval
            elif operator == '>':
                self.val=lval > rval
                return lval > rval
            elif operator == '<':
                self.val=lval < rval
                return lval < rval
            elif operator == '<>':
                self.val= True if lval>rval or lval<rval else False
                return self.val
            else:
                raise Exception('Syntax error')
        except:
            raise Exception('Semantic error')
        

class Operation(Node):
    def _init_(self, operator,lval,rval):
        self.operator = operator
        self.lval = lval
        self.rval = rval
        self.val=0

    def eval(self):
        if isinstance(self.lval,Node):
            lval=self.lval.eval()
        if isinstance(self.rval,Node):
            rval=self.rval.eval()
        operator=self.operator
        if isinstance(lval,(int,float)) and isinstance(rval,(int,float)):
            pass
        elif isinstance(lval,str) and isinstance(rval,str) and operator=='+':
            pass
        elif isinstance(lval,list) and isinstance(rval,list):
            pass
        else:
            raise Exception('Semantic error')
        try:  
            if operator=='+':
                self.val=lval+rval
            elif operator=='-':
                self.val=lval-rval
            elif operator=='*':
                self.val=lval*rval
            elif operator=='/':
                self.val=lval/rval
            elif operator=='div':
                self.val=lval//rval
            elif operator=='mod':
                self.val=lval%rval
            elif operator=='**':
                self.val=lval**rval
            else:
                raise Exception('Syntax error')
            return self.val
        except:
             raise Exception('Semantic error')

class Identifier(Node):
    def _init_(self,id):
        self.id=id

    def eval(self):
        global id_table
        temp=id_table[self.id]
        if isinstance(temp,Node):
            return id_table[self.id]
        else:
            return id_table[self.id]

class Assign(Node):
    def _init_(self, lval, rval):
        self.lval = lval
        self.rval = rval

    def eval(self):
        global id_table
        if isinstance(self.lval, Identifier):
            pass
        else:
            raise Exception('Semantic error')
        lval=self.lval.id
        rval=self.rval.eval()
        id_table[lval]=rval
        
class AssignIndex(Node):
    def _init_(self, lval, rval, index):
        self.lval = lval
        self.rval = rval
        self.index =index

    def eval(self):
        lval=self.lval.id
        rval=self.rval.eval()
        if isinstance(self.index,Node):
            index=self.index.eval()
        else:
            index=self.index
        global id_table
        id_table[lval][index]=rval

class Block(Node):
    def _init_(self, li):
        self.li=li

    def eval(self):
        for st in self.li:
            if isinstance(st,Node):
                st.eval()

class While(Node):
    def _init_(self,cond,exprs):
        self.cond=cond
        self.exprs=exprs
    
    def eval(self):
        while self.cond.eval():
            if isinstance(self.exprs,Node):
                self.exprs.eval()

class Print(Node):
    def _init_(self,val):
        self.val=val
    
    def eval(self):
        if isinstance(self.val,Node):
            print(self.val.eval())
        else:
            print(self.val)
    
class IfElse(Node):
    def _init_(self,ifblock,cond):
        self.ifblock=ifblock
        self.cond=cond
        self.elseblock=None
    
    def setElse(self,elseblock):
        self.elseblock=elseblock

    def eval(self):
        cond=self.cond.eval()
        
        if cond:
            self.ifblock.eval()
        elif self.elseblock is not None:
            if not cond:
                self.elseblock.eval()

class IndexNode(Node):
    def _init_(self,node,index):
        self.node=node
        self.index=index
    
    def eval(self):
        if isinstance(self.node,Node):
            return  self.node.eval()[self.index.eval()] if isinstance(self.index,Node) else self.node.eval()[self.index]
        else:
            return  self.node[self.index.eval()] if isinstance(self.index,Node) else self.node[self.index]

tokens = (
    'Number','Boolean','String','Integer','Real',
    'Print','While','If','Else',
    'LBracket','RBracket','LSQBracket','RSQBracket','LFLRBracket','RFLRBracket',
    'LT','GT','LE','GE','EE','LG',
    'Plus','Minus','Slash','Star','Mod','Div','Comma','Scolon','Cons','Hash','In','Expo',
    'Andalso','Orelse','Not',
    'equal',
    'Identifier'
)

t_LBracket=r'\('
t_RBracket=r'\)'
t_LSQBracket=r'\['
t_RSQBracket=r'\]'
t_LFLRBracket=r'\{'
t_RFLRBracket=r'\}'
t_LT=r'<'
t_GT=r'>'
t_LE=r'<='
t_GE=r'>='
t_EE=r'=='
t_LG=r'<>'
t_Plus=r'\+'
t_Minus=r'\-'
t_Slash=r'/'
t_Star=r'\*'
t_Comma=r','
t_Scolon=r';'
t_Cons=r'::'
t_Hash=r'[#]'
t_Expo= r'\\'
t_equal=r'\='


t_ignore = " \t"

def t_Number(t):
    r'(([-+]?\d*\.\d+|\d+)([eE][+-]?\d+)?)'
    t.value = NumberNode(t.value)
    return t

def t_String(t):
    r'(\').?(\') | (\").?(\")'
    k=t.value[1:-1]
    t.value = StringNode(k)
    return t

def t_Boolean(t):
    r'(True)|(False)'
    t.value = BooleanNode(t.value)
    return t


def t_Identifier(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    global reserved_keywords
    if t.value not in reserved_keywords:
        t.value=Identifier(t.value)
    else:
        t.type=reserved_keywords[t.value]
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
    t.lexer.skip(1)

precedence = (
    ('left', 'Orelse'),
    ('left', 'Andalso'),
    ('left', 'Not'),
    ('left', 'LT','GT','LE','GE','EE','LG'),
    ('right', 'Cons'),
    ('left', 'In'),
    ('left', 'Plus', 'Minus'),
    ('left', 'Star', 'Slash', 'Div', 'Mod'),
    ('right', 'Unary'),
    ('right', 'Expo'),
    ('left', 'Hash'),
    ('left', 'LSQBracket', 'RSQBracket'),
)

def p_block(p):
    '''block : LFLRBracket block RFLRBracket'''
    p[0] = p[1]

def p_block_stmts(p):
    '''block : LFLRBracket stlist RFLRBracket
             | LFLRBracket RFLRBracket'''
    p[0] = Block(p[2].val) if len(p)>3 else Block([])

def p_stlist(p):
    '''stlist : stlist stmt
              | stmt'''
    if len(p)>2:
        p[1].add(p[2])
        p[0]=p[1]
    else:
        temp=ListNode(None)
        temp.add(p[1])
        p[0] = temp

def p_stmt(p):
    '''stmt : singlest Scolon
            | multst'''
    p[0]=p[1]

def p_stmt_singlest(p):
    '''singlest : expr'''
    p[0]=p[1]

def p_varaasign(p):
    '''singlest : Identifier equal expr'''
    if isinstance(p[1],Identifier):
        pass
    else:
        raise Exception('Syntax error')
    p[0]=Assign(p[1],p[3])

def p_varaasign_index(p):
    '''singlest : expr LSQBracket expr RSQBracket equal expr'''
    p[0]=AssignIndex(p[1],p[6],p[3])

def p_print_st(p):
    '''singlest : Print LBracket expr RBracket'''
    p[0]=Print(p[3])

def p_while_stmt(p):
    '''multst : While LBracket expr RBracket block'''
    p[0] = While(p[3], p[5])

def p_conditional_stmt(p):
    '''multst : If LBracket expr RBracket block
              | If LBracket expr RBracket block Else block'''
    
    temp=IfElse(p[5],p[3])
    if len(p)>6:
        temp.setElse(p[7]) 
    p[0]=temp

def p_assgn_var(p):
    '''assgn : Boolean
             | Integer
             | String
             | Real
             | Number
             | Identifier'''
    p[0] = p[1]

def p_expr_assgn(p):
    '''expr : assgn'''
    p[0]=p[1]

def p_expr_bracks(p):
    '''expr : LBracket expr RBracket'''
    p[0] = p[2]

def p_expr_in(p):
    '''expr : expr In expr'''
    if isinstance(p[3],(ListNode,StringNode)):
        pass
    else:
        raise Exception('Semantic error') 
    p[0] = p[3].iscontains(p[1])

def p_expr_operation(p):
    '''expr : expr Plus expr
            | expr Minus expr
            | expr Star expr
            | expr Slash expr
            | expr Expo expr
            | expr Div expr
            | expr Mod expr
            '''
    p[0] = Operation(p[2], p[1], p[3])

def p_expr_unarymin(p):
    '''expr : Minus expr %prec Unary'''
    p[0] = NumberNode(str(-p[2].eval()))

def p_expr_comparison(p):
    '''expr : expr LT expr
            | expr GT expr
            | expr LE expr
            | expr GE expr
            | expr LG expr
            | expr EE expr
            '''
    p[0] = Comparision(p[2], p[1], p[3])

def p_expr_index(p):
    '''expr : expr LSQBracket expr RSQBracket'''
    p[0]=IndexNode(p[1],p[3])

def p_expr_tuple_index(p):
    '''expr : Hash expr expr'''
    p[0]=IndexNode(p[3],p[2])

def p_expr_not(p):
    '''expr : Not expr'''
    p[0]= BooleanNode(p[2],True)

def p_expr_logical(p):
    '''expr : expr Andalso expr
            | expr Orelse expr
            '''
    p[0] = Logical(p[2], p[1], p[3])

def p_exp_list(p):
    '''assgn : LSQBracket li_elems RSQBracket
            | LSQBracket RSQBracket'''
    p[0] = ListNode(None) if len(p)<=3 else p[2]

def p_expr_li_elems(p):
    '''li_elems : li_elems Comma expr
                | expr'''
    if len(p)>2:
        p[1].add(p[3])
        p[0]=p[1]
    else:
        temp=ListNode(None)
        temp.add(p[1])
        p[0] = temp

def p_list_cons(p):
    '''expr : expr Cons expr'''
    if isinstance(p[3],ListNode):
        pass
    else:
        raise Exception('Semantic error')
    p[0]=p[3].pappend(p[1])

def p_exp_tuple(p):
    '''assgn : LBracket tpl_elems RBracket
            | LBracket RBracket'''
    p[0] = TupleNode(p[2].val) if len(p)>3 else TupleNode([])

def p_expr_tpl_elems(p):
    '''tpl_elems : tpl_elems Comma expr
                | expr'''

    if len(p) > 2:
        p[1].add(p[3])
        p[0] = p[1]
    else:
        temp=ListNode(None)
        temp.add(p[1])
        p[0] = temp

def p_error(p):
    print(p)
    raise Exception('Syntax error')

if len(sys.argv)!=2:
    print("Please check the args.")
    exit(1)

debug=False
parser=yacc.yacc(debug=debug)
lexer = lex.lex(debug=debug)
lines=None
with open(sys.argv[1], 'r') as inputfile:
    lines = inputfile.read()

res = parser.parse(lines, debug=0).eval()
# try:
#     res = parser.parse(lines, debug=0).eval()
# except Exception as s:
#     print(str(s).upper())