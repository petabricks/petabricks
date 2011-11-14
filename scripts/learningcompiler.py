#!/usr/bin/python
"""This script compiles multiple instances of a program trying out different
heuristics, and storing in the database the best one that is found"""
import sys
import os
import shutil
import sqlite3
import random
import xml.dom.minidom
import re
import pbutil
import tunerwarnings
import maximaparser
from candidatetester import Candidate
from xml.sax.saxutils import escape
from sgatuner import autotune
from tunerconfig import config

#--------- Config ------------------
conf_deleteTempDir = True
conf_minTrialNumber = 10
conf_probabilityExploration = 0.7
conf_pickBestN = 3
conf_timeout = 5*60
conf_heuristicsFileName = "heuristics.txt"
conf_maxTime = 10 #Seconds

class FailedCandidate:
  """Represents a candidate that failed during compilation or tuning"""
  def __init__(self, heuristicSet=None):
    if heuristicSet is None:
      self.heuristicSet = HeuristicSet()
    else:
      self.heuristicSet = heuristicSet
      
    self.originalIndex=None
    self.failed = True
  
  
    
class HeuristicDB:
  def __init__(self):
    #Open DB    
    try:
      self.__db = sqlite3.connect(self.computeDBPath())
    except:
      self.__db = sqlite3.connect(":memory:")
    self.__createTables()
    self.__bestNCache= dict()
    
  def __createTable(self, name, params):
    cur = self.__db.cursor()
    query = "CREATE TABLE IF NOT EXISTS '"+name+"' "+params
    cur.execute(query)
    cur.close()
    self.__db.commit()
    
  def __createTables(self):
    self.__createTable("HeuristicKind", "('ID' INTEGER PRIMARY KEY AUTOINCREMENT, "
                                        "'name' TEXT UNIQUE)")
    self.__createTable("Heuristic", "('kindID' INTEGER, 'formula' TEXT, "
                                    "'useCount' INTEGER, 'score' FLOAT,"
                                    "PRIMARY KEY (kindID, formula), "
                                    "FOREIGN KEY ('kindID') REFERENCES 'HeuristicKind' ('ID')"
                                    "ON DELETE CASCADE ON UPDATE CASCADE)")
    #TODO:self.__createTable("InSet", "('setID' INTEGER, 'heuristicID' INTEGER)"
    
  def computeDBPath(self):
    #TODO: make the path more flexible
    dbPath= os.path.expanduser(config.output_dir+"/knowledge.db")
    return dbPath

  def getHeuristicKindID(self, kindName):
    cur = self.__db.cursor()
    query = "SELECT ID From HeuristicKind WHERE name='"+kindName+"'"
    cur.execute(query)
    kindID = cur.fetchone()[0]
    cur.close()
    return kindID
    
  def storeHeuristicKind(self, kindName):
    cur = self.__db.cursor()
    query = "INSERT OR IGNORE INTO HeuristicKind ('name') VALUES ('"+kindName+"')"
    cur.execute(query)
    cur.close()
    self.__db.commit()
    return self.getHeuristicKindID(kindName)
    
  def increaseHeuristicScore(self, name, formula, score):
    kindID=self.storeHeuristicKind(name) 
    cur = self.__db.cursor()
    query = "UPDATE Heuristic SET score=score+? WHERE kindID=? AND formula=?"
    cur.execute(query, (score, kindID, formula))
    if cur.rowcount == 0:
      #There was no such heuristic in the DB: probably it was taken from the defaults
      query = "INSERT INTO Heuristic (kindID, formula, useCount, score) VALUES (?, ?, 1, ?)"
      cur.execute(query, (kindID, formula, score))
    cur.close()
    self.__db.commit()
  
  def increaseHeuristicUseCount(self, name, formula):
    kindID=self.storeHeuristicKind(name) 
    cur = self.__db.cursor()
    query = "UPDATE Heuristic SET useCount=useCount+1 WHERE kindID=? AND formula=?"
    cur.execute(query, (kindID, formula))
    if cur.rowcount == 0:
      #There was no such heuristic in the DB: let's add it
      query = "INSERT INTO Heuristic (kindID, formula, useCount, score) VALUES (?, ?, 1, 0)"
      cur.execute(query, (kindID, formula))
    cur.close()
    self.__db.commit()
  
  def increaseScore(self, hSet, score):
    """Increase the score of a set of heuristics by the given amount"""
    #TODO: also store it as a set
    for name, formula in hSet.iteritems():
      self.increaseHeuristicScore(name, formula, score)
      
  def markAsUsed(self, hSet):
    """Mark a set of heuristics as used for generating a candidate executable"""
    #TODO: also store it as a set
    for name, formula in hSet.iteritems():
      self.increaseHeuristicUseCount(name, formula)

  def getBestNHeuristics(self, name, N):
    try:
      cached = self.__bestNCache[name]
      return cached
    except:
      #Not in the cache
      #Fall back to accessing the db
      pass
    cur = self.__db.cursor()
    query = "SELECT formula FROM Heuristic JOIN HeuristicKind ON Heuristic.kindID=HeuristicKind.ID WHERE HeuristicKind.name=? ORDER BY Heuristic.score/Heuristic.useCount DESC LIMIT ?"
    cur.execute(query, (name, N))
    result = [row[0] for row in cur.fetchall()]
    cur.close()
    self.__bestNCache[name]=result
    return result
    
    
    
    
  
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
  
  def importFromXml(self, xmlDOM):
    for heuristicXML in xmlDOM.getElementsByTagName("heuristic"):
      name=heuristicXML.getAttribute("name")
      formula=heuristicXML.getAttribute("formula")
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
      bestN=db.getBestNHeuristics(heuristic, N)
      if len(bestN) == 0:
        #No such heuristic in the DB. Do not complete the set
        #This is not a problem. It's probably a new heuristic:
        #just ignore it and it will fall back on the default implemented 
        #into the compiler
        continue
      formula=random.choice(bestN)
      
      if random.random() < conf_probabilityExploration:
        #Generete a new formula by modifying the existing one
        formulaObj = maximaparser.parse(formula)
        formulaObj.evolveValue()
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
  def __init__(self, heuristicSetFileName=None):
    self.__heuristicSets = []
    if heuristicSetFileName is not None:
      self.__xml = xml.dom.minidom.parse(heuristicSetFileName)
      
      # Extract information
      for hSet in self.__xml.getElementsByTagName("set"):
        self.__heuristicSets.append(self.__parseHeuristicSet(hSet))
    
    
  def __parseHeuristicSet(self, hSetXML):
    """Parses a xml heuristic set returning it as a list of pairs name-formula"""
    hSet = HeuristicSet()
    hSet.importFromXml(hSetXML)
    return hSet
     
  def heuristicSet(self, i):
    """Get the i-th heuristic set"""
    return self.__heuristicSets[i]
  
  def allHeuristicSets(self):
    return self.__heuristicSets
    

def candidateKey(candidate):
  """Generates a comparison key for a candidate.
Candidates are sorted by the number of dimensions (the highest, the better),
then by average execution time of the biggest dimension (the lower the better)"""
  if candidate.failed:
    return (float('inf'), float('inf'))
  numDimensions = len(candidate.metrics[0])
  executionTime = candidate.metrics[0][2**(numDimensions-1)].mean()
  return (1/numDimensions, executionTime)



class CandidateList(list):  
  def addOriginalIndex(self):
    count = 0
    for candidate in self:
      candidate.originalIndex = count;
      count = count + 1
      
  def sortBySpeed(self):
    """Adds the "score" and "originalIndex" attributes to every candidate. 
    Also, sorts the list by score"""
    self.sort(key=candidateKey)
        
    
  
    


class LearningCompiler:
  def __init__(self, pbcExe, heuristicSetFileName=None, jobs=None):
    self.__heuristicManager = HeuristicManager(heuristicSetFileName)
    self.__minTrialNumber = conf_minTrialNumber
    self.__pbcExe = pbcExe    
    self.__jobs=jobs    
    self.__db = HeuristicDB()
    random.seed()
    
  
  def storeCandidatesDataInDB(self, candidates, basesubdir, basename):
    """Store data from all the info file, with score.
The candidates should already be ordered (from the best to the worst) and 
with the originalIndex field added"""
    numCandidates = len(candidates)
    count=0
    for candidate in candidates:
      if not candidate.failed:
	infoFile=os.path.join(basesubdir, 
			      str(candidate.originalIndex), 
			      basename+".info")
      
	score = (numCandidates - count) / float(numCandidates)
	
	#Take the data about the used heuristics scores and store it into the DB
	infoxml = xml.dom.minidom.parse(infoFile)
	hSet = HeuristicSet()
	hSet.importFromXml(infoxml)
	self.__db.increaseScore(hSet, score)
	self.__db.markAsUsed(hSet)
      else:
	#Compilation has failed!
	#Take the data about the heuristics from the input heuristics file
	#instead of the info file (because there no such file!). 
	#We are not sure that all the heuristics in the input
	#file have been used, but they had the compilation fail.
	#Their score is not increased, but they are marked as used
	#(and therefore they are penalized)
	self.__db.markAsUsed(candidate.heuristicSet)
	
      count = count +1
      
    
    
  def compileLearningHeuristics(self, benchmark, finalBinary=None):
    #Define the time to spend autotuning each candidate
    config.max_time= conf_maxTime
    
    #Define file names
    path, basenameExt = os.path.split(benchmark)
    if path == "":
      path="./"
    basename, ext = os.path.splitext(basenameExt)
    basesubdir=os.path.join(path,basename+".tmp")
    #Init variables
    candidates=CandidateList()
    
    #Compile with current best heuristics
    outDir = os.path.join(basesubdir, "0")
    if not os.path.isdir(outDir):
      #Create the output directory
      os.makedirs(outDir)
    binary= os.path.join(outDir, basename)  
    status=pbutil.compileBenchmark(self.__pbcExe, benchmark, binary=binary, jobs=self.__jobs)  
    if status != 0:
      print "Compile FAILED with current best heuristics - Compilation aborted"
      return status
      
    try:
      autotune(binary, candidates)
      
      #Candidate has not failed: mark as such
      currentCandidate = candidates[-1]
      currentCandidate.failed = False
      
    except tunerwarnings.AlwaysCrashes:
        print "Current best Candidate always crashes!"
        #Add an empty entry for the candidate
        candidates.append(FailedCandidate())
    
    #Get the full set of heuristics used
    infoFile=binary+".info"
    currentBestHSet = HeuristicSet()
    currentBestHSet.importFromXml(xml.dom.minidom.parse(infoFile))
    neededHeuristics = currentBestHSet.keys()
    
    #Get hSets
    allHSets = self.__heuristicManager.allHeuristicSets()
    while len(allHSets) < (self.__minTrialNumber): #Not enough hSets!
      allHSets.append(HeuristicSet())
    
    numSets = len(allHSets)
    
    
    count=0
    successfulCompilations=1 #The first compilation has already been done
    for hSet in allHSets:
      hSet.complete(neededHeuristics, self.__db, conf_pickBestN)
      count = count + 1
      
      #Define more file names
      outDir = os.path.join(basesubdir, str(count))
      if not os.path.isdir(outDir):
        #Create the output directory
        os.makedirs(outDir)
      binary= os.path.join(outDir, basename)  
      
      heuristicsFile= os.path.join(outDir, conf_heuristicsFileName)
      hSet.toXmlFile(heuristicsFile)
      
      status = pbutil.compileBenchmark(self.__pbcExe, benchmark, binary=binary, heuristics=heuristicsFile, jobs=self.__jobs, timeout=conf_timeout)
      if status != 0:
        print "Compile FAILED while using heuristic set #"+str(count)+": ",
        print hSet
        #Add an empty entry for the candidate
        candidates.append(FailedCandidate(hSet))
        #Go to next heuristic set
        continue
      
      #Autotune
      try:
        autotune(binary, candidates)
        
        #Candidate has not failed: mark as such
	currentCandidate = candidates[-1]
	currentCandidate.failed = False
      except tunerwarnings.AlwaysCrashes:
        print "Candidate "+str(count)+" always crashes!"
        #Add an empty entry for the candidate
        candidates.append(FailedCandidate(hSet))
      
      
      
    candidates.addOriginalIndex()
    candidates.sortBySpeed()
    
    if candidates[0] is None:
      raise tunerwarnings.AlwaysCrashes()
    
    self.storeCandidatesDataInDB(candidates, basesubdir, basename)
    
    bestIndex = candidates[0].originalIndex
    
    print "The best candidate is: "+str(bestIndex)
  
    #Move every file to the right place
    bestSubDir=os.path.join(basesubdir, str(bestIndex))
    #  compiled program:
    bestBin=os.path.join(bestSubDir, basename)
    if finalBinary is not None:
      finalBin=finalBinary
    else:
      finalBin=os.path.join(path, basename)
    shutil.move(bestBin, finalBin)
    #  .cfg file
    bestCfg=os.path.join(bestSubDir, basename+".cfg")
    finalCfg=finalBin + ".cfg"
    shutil.move(bestCfg, finalCfg)
    #  .info file
    bestInfo=os.path.join(bestSubDir, basename+".info")
    finalInfo=finalBin+".info"
    shutil.move(bestInfo, finalInfo)
    #  .obj directory
    bestObjDir=os.path.join(bestSubDir, basename+".obj")
    destObjDir=finalBin+".obj"
    if os.path.isdir(destObjDir):
      shutil.rmtree(destObjDir)
    shutil.move(bestObjDir, destObjDir)
    #  input heuristic file
    if bestIndex != 0: #Program 0 is run with only the best heuristics in the DB
      bestHeurFile=os.path.join(bestSubDir, conf_heuristicsFileName )
      finalHeurFile=finalBin+".heur"
      shutil.move(bestHeurFile, finalHeurFile)
    
    #Delete all the rest
    if conf_deleteTempDir:
      shutil.rmtree(basesubdir)
    
    return 0
    
    
    
    
    

#TEST
if __name__ == "__main__":
  #basedir="/afs/csail.mit.edu/u/m/mtartara/programs/petabricks/"
  basedir="/home/mikyt/programmi/petabricks/"
  pbc=basedir+"src/pbc"
  l=LearningCompiler(pbc, sys.argv[1], conf_minTrialNumber)
  l.compileLearningHeuristics(sys.argv[2])
