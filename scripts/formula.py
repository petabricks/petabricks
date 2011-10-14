import random

_randMIN = -100
_randMAX = 100

class FormulaVariable:
  def __init__(self, ident):
    self.ident=ident

  def __repr__(self):
    return self.ident
    
  def mutateValue(self):
    #No values to mutate in a variable
    return self
  
class FormulaInteger:
  def __init__(self, value):
    self.value=value
    
  def __repr__(self):
    return str(self.value)
    
  def mutateValue(self):
    self.value = random.randint(_randMIN, _randMAX)
    
class FormulaBool:
  def __init__(self, value):
    self.value=value
    
  def __repr__(self):
    if self.value:
      return "true"
    else:
      return "false"
      
  def mutateValue(self):
    if random.random() < 0.5:
      self.value = False
    else:
      self.value = True
      
      
class FormulaFloat:
  def __init__(self, value):
    self.value=value
    
  def __repr__(self):
    return str(self.value)
    
  def mutateValue(self):
    self.value = random.uniform(_randMIN, _randMAX)
    
    
class FormulaBinop:
  def __init__(self, op, left, right):
    self.op=op
    self.left=left
    self.right=right
    
  def __repr__(self):
    return "("+ str(self.left) +" "+ str(self.op) + " " + str(self.right)+")"
    
  def mutateValue(self):
    """Randomly mutates one of the values (int, float, bool) that are in the formula.
If no value is present, nothing is changed.

NB: the selection is completely random, so the value could not change at all!"""
    if random.random() < 0.5:
      self.left.mutateValue()
    else:
      self.right.mutateValue()
      
class FormulaIf:
  def __init__(self, cond, thenClause, elseClause=None):
    self.cond = cond
    self.thenClause = thenClause
    self.elseClause = elseClause
    
  def __repr__(self):
    if self.elseClause is not None:
      elsePart=" else " + str(self.elseClause)
    else:
      elsePart=""
      
    return "if " + str(self.cond) + " then " + str(self.thenClause) + elsePart
    
  def mutateValue(self):
    choice = random.choice(range(3))
    if choice==0:
      self.cond.mutateValue()
    elif choice==1:
      self.thenClause.mutateValue()
    elif choice==2 and elseClause is not None:
      self.elseClause.mutateValue()