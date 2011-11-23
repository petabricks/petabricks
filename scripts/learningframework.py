"""Framework implementing long term learning of heuristics """
import random
import xml.dom.minidom
import maximaparser
import heuristicdb
import os
from xml.sax.saxutils import escape


#---------------- Config ------------------
conf_minTrialNumber = 6
conf_probabilityExploration = 0.7
conf_pickBestN = 3
#------------------------------------------

class FailedCandidate:
  """Represents a candidate that failed during compilation or tuning.
If assignScores is False, when this candidate is graded it's only marked as used
but not given any point (thus it is penalized)"""
  def __init__(self, heuristicSet = None, assignScores = True):
    if heuristicSet is None:
      self.heuristicSet = HeuristicSet()
    else:
      self.heuristicSet = heuristicSet
    
    self.originalIndex = None
    self.failed = True
    self.assignScores = assignScores

    
  
class HeuristicSet(dict):
  def toXmlStrings(self):
    return ["<heuristic name=\""+name+"\" formula=\""+escape(self[name])+"\" />" for name in self]
  
  def toXmlFile(self, filename):
    outfile = open(filename, "w")
    outfile.write("<heuristics>\n")
    for xmlstring in self.toXmlStrings():
      outfile.write("\t")
      outfile.write(xmlstring)
      outfile.write("\n")
    outfile.write("</heuristics>\n")
    outfile.close()
  
  def importFromXml(self, xmlFileName):
    self.importFromXmlDOM(xml.dom.minidom.parse(xmlFileName))
    
  def importFromXmlDOM(self, xmlDOM):
    for heuristicXML in xmlDOM.getElementsByTagName("heuristic"):
      name = heuristicXML.getAttribute("name")
      formula = heuristicXML.getAttribute("formula")
      #Use the parser to validate (and to constant fold) the formula
      formula = str(maximaparser.parse(formula))
      self[name] = formula
  
  def complete(self, heuristicNames, db, N):
    """Complete the sets using the given db, so that it contains all the 
heuristics specified in the heuristicNames list.

Every missing heuristic is completed with one randomly taken from the best N 
heuristics in the database  """
    #Find the missing heuristics
    missingHeuristics = list(heuristicNames)
    for name in self:
      try:
        missingHeuristics.remove(name)
      except ValueError:
        #A heuristic could be in the input file, but useless, therefore not in
        #the missingHeuristic list
        pass
      
    #Complete the set
    for heuristic in missingHeuristics:
      bestN = db.getBestNHeuristics(heuristic, N)
      if len(bestN) == 0:
        #No such heuristic in the DB. Do not complete the set
        #This is not a problem. It's probably a new heuristic:
        #just ignore it and it will fall back on the default implemented 
        #into the compiler
        continue
      formula = random.choice(bestN)
      
      if random.random() < conf_probabilityExploration:
        #Generete a new formula by modifying the existing one
        formulaObj = maximaparser.parse(formula)
        formulaObj.evolve()
        formula = str(formulaObj)
        
      self[heuristic] = formula
  
  
  
    
class HeuristicManager:
  """Manages sets of heuristics stored in a file with the following format:
<heuristics>
  <set>
    <heuristic name="heuristicName" formula="a+b+c" />
    <heuristic name="heuristicName2" formula="a+b+d" />
  </set>
  <set>
    <heuristic name="heuristicName3" formula="x+y*z" />
    <heuristic name="heuristicName4" formula="a+g+s" />
  </set>
</heuristics>
"""
  def __init__(self, heuristicSetFileName = None):
    self._heuristicSets = []
    if heuristicSetFileName is not None:
      self._xml = xml.dom.minidom.parse(heuristicSetFileName)
      
      # Extract information
      for hSet in self._xml.getElementsByTagName("set"):
        self._heuristicSets.append(self._parseHeuristicSet(hSet))
    
    
  def _parseHeuristicSet(self, hSetXML):
    """Parses a xml heuristic set returning it as a list of pairs name-formula"""
    hSet = HeuristicSet()
    hSet.importFromXmlDOM(hSetXML)
    return hSet
     
  def heuristicSet(self, i):
    """Get the i-th heuristic set"""
    return self._heuristicSets[i]
  
  def allHeuristicSets(self):
    return self._heuristicSets
    





class CandidateList(list):  
  def __init__(self, sortingKey):
    self._sortingKey = sortingKey
    
  def addOriginalIndex(self):
    count = 0
    for candidate in self:
      candidate.originalIndex = count;
      count = count + 1
      
  def sort(self):
    #Call sort() of "list"
    super(CandidateList, self).sort(key = self._sortingKey)
        
    
  
    


class Learner:
  def __init__(self, 
               testHSet, 
               candidateSortingKey,
               heuristicSetFileName = None, 
               setup = None, 
               tearDown = None,
               getNeededHeuristics = None):
    self._heuristicManager = HeuristicManager(heuristicSetFileName)
    self._minTrialNumber = conf_minTrialNumber
    self._db = heuristicdb.HeuristicDB()
    self._setup = setup
    self._testHSet = testHSet
    self._candidateSortingKey = candidateSortingKey
    self._tearDown = tearDown
    self._getNeededHeuristcs = getNeededHeuristics
    random.seed()
    
  
  def storeCandidatesDataInDB(self, candidates):
    """Store data from all the info file, with score.
The candidates should already be ordered (from the best to the worst) and 
with the originalIndex field added"""
    numCandidates = len(candidates)
    count = 0
    for candidate in candidates:
      score = (numCandidates - count) / float(numCandidates)
      self._db.markAsUsed(candidate.heuristicSet)
      
      if candidate.assignScores:
        self._db.increaseScore(candidate.heuristicSet, score)
        
      count = count +1
      
    
    
  def learnHeuristics(self, benchmark):
    #Init variables
    candidates = CandidateList(self._candidateSortingKey)
    additionalParameters={}
    
    additionalParameters["candidates"] = candidates
    
    if self._setup is not None:
      result = self._setup(benchmark, additionalParameters) 
      if result != 0:
        return result
      
    #Get heuristic sets
    allHSets = self._heuristicManager.allHeuristicSets()
    while len(allHSets) < (self._minTrialNumber): #Not enough hSets!
      allHSets.append(HeuristicSet())
    
    #Complete heuristic sets
    neededHeuristics = self._getNeededHeuristcs(benchmark)
    
    for hSet in allHSets:
      hSet.complete(neededHeuristics, self._db, conf_pickBestN)
    
    count = 0
    for hSet in allHSets:
      count = count + 1
      currentCandidate = self._testHSet(benchmark, count, hSet, additionalParameters) 
      candidates.append(currentCandidate)
      
    candidates.addOriginalIndex()
    candidates.sort()
    
    if candidates[0].failed:
      print "ALL candidates crash!"
      return -1
    
    self.storeCandidatesDataInDB(candidates)
    
    self._tearDown(benchmark, additionalParameters)
    
    return 0
    
