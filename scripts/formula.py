import random

_mutateMIN = -100
_mutateMAX = 100
_mutationProbability = 0.3

def isImmediateNumber(formula):
  return isinstance(formula, FormulaInteger) or \
         isinstance(formula, FormulaFloat)


class FormulaVariable:
  def __init__(self, ident):
    self.ident=ident

  def __repr__(self):
    return self.ident
    
  def evolve(self):
    #No values to mutate in a variable
    return False
  
  
class FormulaInteger:
  def __init__(self, value):
    self.value=value
    
  def __repr__(self):
    return str(self.value)
    
  def evolve(self):
    if random.random() < _mutationProbability:
      self.mutateValue()
      return True
    #Evolve (=increment/decrement of at most 100% of the original value)
    oldValue = self.value 
    self.value = int(self.value + (self.value * random.uniform(-1, 1)))
    if oldValue == self.value:
      #The value did not change. It was too small to be modified that way
      #Force a change
      if random.random() < 0.5:
        self.value = self.value - 1
      else:
        self.value = self.value + 1
    return True

  def mutateValue(self):
    """Get a completely random new value"""
    oldValue = self.value 
    while oldValue==self.value:
      self.value = random.randint(_mutateMIN, _mutateMAX)
    return True


class FormulaBool:
  def __init__(self, value):
    self.value=value
    
  def __repr__(self):
    if self.value:
      return "true"
    else:
      return "false"
      
  def evolve(self):
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
    
  def evolve(self):
    if random.random() < _mutationProbability:
      self.mutateValue()
      return True
    #Evolve (=increment/decrement of at most 100% of the original value)
    oldValue = self.value 
    self.value = self.value + (self.value * random.uniform(-1, 1))
    if oldValue == self.value:
      #The value did not change. It was too small to be modified that way
      #Force a change
      if random.random() < 0.5:
        self.value = self.value - 1
      else:
        self.value = self.value + 1
    return True
    
  def mutateValue(self):
    oldValue = self.value
    while(oldValue==self.value):
      self.value = random.uniform(_mutateMIN, _mutateMAX)
    return True
    
    
class NoElementException(Exception):
  pass

def selectDifferentElement(element, theList):
  """Select an element from the list, different from the given one"""
  if len(theList) == 1 and element==theList[0]:
    raise NoElementException()
  
  newElement = random.choice(theList)
  while newElement == element:
    newElement = random.choice(theList)
    
  return newElement
  
  
  
    
class FormulaBinop:
  def __init__(self, op, left, right):
    self.op=op
    self.left=left
    self.right=right
    
  def __repr__(self):
    reprStr = "("+ str(self.left) +" "+ str(self.op) + " " + str(self.right)+")"
    if not (isImmediateNumber(self.left) and isImmediateNumber(self.right)):
      #Return extended representation
      return reprStr
    else:
      #Constant folding
      #Handle special cases where sintax is different
      if self.op == "=":
	op = "=="
	reprStr = "("+ str(self.left) +" "+ str(op) + " " + str(self.right)+")"
      
      return str(eval(reprStr))
    
  def evolveValue(self):
    """Randomly mutates one of the values (int, float, bool) that are in the 
formula.
If no value is present, nothing is changed.

The formula is mutated in place.

Returns true if the formula was actually mutated, false otherwise"""
    mutated = False
    if random.random() < 0.5:
      mutated = self.left.evolve()
      if not mutated:
        mutated = self.right.evolve()
    else:
      mutated = self.right.evolve()
      if not mutated:
        mutated = self.left.evolve()
    
    return mutated
  
  
  def evolveOperator(self):
    comparison_operators=["=", "!=", "<", ">", ">=", "<="]
    binary_logic_operators=["and", "or"]
    arithmetic_operators=["+", "-", "*", "/"]
    
    try:
      if self.op in comparison_operators:
        self.op = selectDifferentElement(self.op, comparison_operators)
      elif self.op in binary_logic_operators:
        self.op = selectDifferentElement(self.op, binary_logic_operators)
      elif self.op in arithmetic_operators:
        self.op = selectDifferentElement(self.op, arithmetic_operators)
      else:
        raise Exception("Unknown operator: " + self.op)
    except NoElementException:
      return False
       
    return True
        
  def evolve(self):
    """Randomly mutate one of the values or the binary operation, or the 
operator"""
    choices=range(2)
    random.shuffle(choices)
    for choice in choices:
      mutated=False
      if choice==0:
        mutated=self.evolveValue()
      elif choice==1:
        mutated=self.evolveOperator()
      
      if mutated:
        return True
    
    return False
      
      
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
    
  def evolve(self):
    choices = range(3)
    random.shuffle(choices)
    for choice in choices:
      mutated = False
      if choice==0:
        mutated = self.cond.evolve()
      elif choice==1:
        mutated = self.thenClause.evolve()
      elif choice==2 and self.elseClause is not None:
        mutated = self.elseClause.evolve()
      
      if mutated:
        return True
    
    return False