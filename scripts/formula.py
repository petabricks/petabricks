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
    return False
  
  
class FormulaInteger:
  def __init__(self, value):
    self.value=value
    
  def __repr__(self):
    return str(self.value)
    
  def mutateValue(self):
    oldValue = self.value 
    while oldValue==self.value:
      self.value = random.randint(_randMIN, _randMAX)
    return True


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
    return True  
      
      
class FormulaFloat:
  def __init__(self, value):
    self.value=value
    
  def __repr__(self):
    return str(self.value)
    
  def mutateValue(self):
    oldValue = self.value
    while(oldValue==self.value):
      self.value = random.uniform(_randMIN, _randMAX)
    return True
    
    
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

The formula is mutated in place.

Returns true if the formula was actually mutated, false otherwise"""
    mutated = False
    if random.random() < 0.5:
      mutated = self.left.mutateValue()
      if not mutated:
        mutated = self.right.mutateValue()
    else:
      mutated = self.right.mutateValue()
      if not mutated:
        mutated = self.left.mutateValue()
    
    return mutated
    
    
      
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
    choices = range(3)
    random.shuffle(choices)
    for choice in choices:
      mutated = False
      if choice==0:
        mutated = self.cond.mutateValue()
      elif choice==1:
        mutated = self.thenClause.mutateValue()
      elif choice==2 and self.elseClause is not None:
        mutated = self.elseClause.mutateValue()
      
      if mutated:
        return True
    
    return False