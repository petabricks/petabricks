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
conf_probabilityExploration = 1
conf_pickBestN = 3
#--------- Autotuner config --------
config.max_time=10 #Seconds
#-----------------------------------





class HeuristicDB:
  def __init__(self):
    #Open DB    
    self.__db = sqlite3.connect(self.computeDBPath())
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
                                    "'useCount' INTEGER, 'bestCount' INTEGER,"
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
    
  def increaseHeuristicBestCount(self, name, formula):
    kindID=self.storeHeuristicKind(name) 
    cur = self.__db.cursor()
    query = "UPDATE Heuristic SET bestCount=bestCount+1 WHERE kindID=? AND formula=?"
    cur.execute(query, (kindID, formula))
    if cur.rowcount == 0:
      #There was no such heuristic in the DB: is should be present as USED!!
      raise Exception("The following formula was not present in the DB: \n"+formula)
    cur.close()
    self.__db.commit()
  
  def increaseHeuristicUseCount(self, name, formula):
    kindID=self.storeHeuristicKind(name) 
    cur = self.__db.cursor()
    query = "UPDATE Heuristic SET useCount=useCount+1 WHERE kindID=? AND formula=?"
    cur.execute(query, (kindID, formula))
    if cur.rowcount == 0:
      #There was no such heuristic in the DB: let's add it
      query = "INSERT INTO Heuristic (kindID, formula, useCount, bestCount) VALUES (?, ?, 1, 0)"
      cur.execute(query, (kindID, formula))
    cur.close()
    self.__db.commit()
  
  def markAsBest(self, hSet):
    """Mark a set of heuristics as selected as the best one for an executable"""
    #TODO: also store it as a set
    for name, formula in hSet.iteritems():
      self.increaseHeuristicBestCount(name, formula)
      
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
    query = "SELECT formula FROM Heuristic JOIN HeuristicKind ON Heuristic.kindID=HeuristicKind.ID WHERE HeuristicKind.name=? ORDER BY Heuristic.bestCount/Heuristic.useCount DESC LIMIT ?"
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
    random.seed()
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
    

  
  
    


class LearningCompiler:
  def __init__(self, pbcExe, heuristicSetFileName=None, jobs=None):
    self.__heuristicManager = HeuristicManager(heuristicSetFileName)
    self.__minTrialNumber = conf_minTrialNumber
    self.__pbcExe = pbcExe    
    self.__jobs=jobs
    
  def bestCandidate(self, candidates):
    """Determines which candidate is the best, given a set of candidates with 
all their timings

Returns the index of the best candidate in the array"""
    #Get the maximum number of dimensions candidates have
    maxDimensions = 0
    fasterCandidates = {}
    count=0
    for candidate in candidates:
      if candidate is None:
        #This candidate has failed the autotuning
        continue
      
      candidateDimensions=len(candidate.metrics[0])
      if candidateDimensions > maxDimensions:
        fasterCandidates = {}
      if candidateDimensions >= maxDimensions:
        maxDimensions = candidateDimensions
        fasterCandidates[candidate] = count
      count = count+1

    if len(fasterCandidates) == 1:
      #One candidate has analyzed more dimensions than the others:
      #it was faster!
      return fasterCandidates[fasterCandidates.keys()[0]]
      
    #Select best candidate:
    #the one on average faster on the biggest dimension
    bestScore = float("inf")
    bestIndex = None
    for candidate in fasterCandidates.keys():
      timingResultDB = candidate.metrics[0] #Get the 'timings' metric
      #The score for each candidate is the average timing on the highest shared 
      #dimension
      results = timingResultDB[2**(maxDimensions-1)]
      average = results.mean()
      if average < bestScore:
        bestScore=average
        bestIndex=fasterCandidates[candidate]
    
    if bestIndex==None:
      raise tunerwarnings.AlwaysCrashes()
    
    return bestIndex
    
    

  def compileLearningHeuristics(self, benchmark, finalBinary=None):
    #Define file names
    path, basenameExt = os.path.split(benchmark)
    if path == "":
      path="./"
    basename, ext = os.path.splitext(basenameExt)
    basesubdir=os.path.join(path,basename+".tmp")
    #Init variables
    candidates=[]
    db = HeuristicDB()
    
    #Compile with current best heuristics
    outDir = os.path.join(basesubdir, "0")
    if not os.path.isdir(outDir):
      #Create the output directory
      os.makedirs(outDir)
    binary= os.path.join(outDir, basename)  
    status=pbutil.compileBenchmark(self.__pbcExe, benchmark, binary=binary, jobs=self.__jobs)  
    if status != 0:
      return status
      
    try:
      autotune(binary, candidates)
    except tunerwarnings.AlwaysCrashes:
        print "Current best Candidate always crashes!"
        #Add an empty entry for the candidate
        candidates.append(None)
    
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
    
    
    count=1
    for hSet in allHSets:
      hSet.complete(neededHeuristics, db, conf_pickBestN)
      
      #Define more file names
      outDir = os.path.join(basesubdir, str(count))
      if not os.path.isdir(outDir):
        #Create the output directory
        os.makedirs(outDir)
      binary= os.path.join(outDir, basename)  
      
      heuristicsFile= os.path.join(outDir, "heuristics.txt")
      hSet.toXmlFile(heuristicsFile)
      
      status = pbutil.compileBenchmark(self.__pbcExe, benchmark, binary=binary, heuristics=heuristicsFile, jobs=self.__jobs)
      if status != 0:
        print "Compile FAILED"
        print "while using heuristics: "
        print hSet
        return status
        
      #Get the actual used hset (including default formulas from the 
      #compiler itself, if any)
      infoxml = xml.dom.minidom.parse(binary+".info")
      usedHSet = HeuristicSet()
      usedHSet.importFromXml(infoxml)
      db.markAsUsed(usedHSet)
      
      #Autotune
      try:
        autotune(binary, candidates)
      except tunerwarnings.AlwaysCrashes:
        print "Candidate "+str(count)+" always crashes!"
        #Add an empty entry for the candidate
        candidates.append(None)
      
      count = count + 1
      
    bestIndex = self.bestCandidate(candidates)
    
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
      bestHeurFile=os.path.join(bestSubDir, "heuristics.txt")
      finalHeurFile=finalBin+".heur"
      shutil.move(bestHeurFile, finalHeurFile)
    
    #Delete all the rest
    if conf_deleteTempDir:
      shutil.rmtree(basesubdir)
    
    #Take the data about the best heuristics and store it into the db
    infoxml = xml.dom.minidom.parse(finalInfo)
    hSet = HeuristicSet()
    hSet.importFromXml(infoxml)
    db.markAsBest(hSet)
    
    return 0
    
    
    
    
    

#TEST
if __name__ == "__main__":
  #basedir="/afs/csail.mit.edu/u/m/mtartara/programs/petabricks/"
  basedir="/home/mikyt/programmi/petabricks/"
  pbc=basedir+"src/pbc"
  l=LearningCompiler(pbc, sys.argv[1], conf_minTrialNumber)
  l.compileLearningHeuristics(sys.argv[2])
