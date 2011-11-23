#!/usr/bin/python
"""Framework implementing long term learning of heuristics for the PetaBricks
compiler

Michele Tartara <mikyt@users.sourceforge.net>"""

import learningframework
import os
import pbutil
import tunerwarnings
import shutil
import sys
from tunerconfig import config
from sgatuner import autotune
from candidatetester import Candidate

#------------------ Config --------------------
CONF_MAX_TIME = 10 #Seconds
CONF_DELETE_TEMP_DIR = True
CONF_TIMEOUT = 5*60
CONF_HEURISTIC_FILE_NAME = "heuristics.txt"
#----------------------------------------------

def candidateKey(candidate):
  """Generates a comparison key for a candidate.
Candidates are sorted by the number of dimensions (the highest, the better),
then by average execution time of the biggest dimension (the lower the better)"""
  if candidate.failed:
    return (float('inf'), float('inf'))
  numDimensions = len(candidate.metrics[0])
  executionTime = candidate.metrics[0][2**(numDimensions-1)].mean()
  return (1/numDimensions, executionTime)
  
  

class LearningCompiler:
  def __init__(self, pbcExe, heuristicSetFileName = None, jobs = None):
    self._learner = learningframework.Learner(self.testHSet,
                                              candidateKey,
                                              heuristicSetFileName,
                                              self.setup,
                                              self.tearDown,
                                              self.getNeededHeuristics)
    self._pbcExe = pbcExe    
    self._jobs = jobs
    self._neededHeuristics={}
    
    
  def compileLearningHeuristics(self, benchmark, finalBinary = None):
    #Define the time to spend autotuning each candidate
    config.max_time = CONF_MAX_TIME
    self._finalBinary = finalBinary
    
    return self._learner.learnHeuristics(benchmark)
    
  
  def testHSet(self, benchmark, count, hSet, additionalParameters):
    """Return the object representing a tested candidate, with (at least) the 
following attributes:
  
  * failed (bool): has the candidate failed the compilation/testing process?
  * assignScores (bool): should the score be assigned to the candidate normally
                         or should we just mark it as used (thus penalizing it)
  * heuristicSet (HeuristicSet): the set of heuristics that generated the 
                                 program"""
    candidates=[]
    candidate = None
    basesubdir = additionalParameters["basesubdir"]
    basename = additionalParameters["basename"]
    
    #Define more file names
    
    outDir = os.path.join(basesubdir, str(count))
    if not os.path.isdir(outDir):
      #Create the output directory
      os.makedirs(outDir)
    binary= os.path.join(outDir, basename)  
    
    heuristicsFile= os.path.join(outDir, CONF_HEURISTIC_FILE_NAME)
    hSet.toXmlFile(heuristicsFile)
    
    status = pbutil.compileBenchmark(self._pbcExe, 
                                     benchmark, 
                                     binary = binary, 
                                     heuristics = heuristicsFile, 
                                     jobs = self._jobs, 
                                     timeout = CONF_TIMEOUT)
    if status != 0:
      #Compilation has failed!
      #Take the data about the heuristics from the input heuristics file
      #instead of the info file (because there no such file!). 
      #We are not sure that all the heuristics in the input
      #file have been used, but they had the compilation fail.
      #Their score is not increased, but they are marked as used
      #(and therefore they are penalized)
      print "Compile FAILED while using heuristic set #"+str(count)+": ",
      print hSet
      return learningframework.FailedCandidate(hSet, assignScores = False)
      
        
    #Autotune
    try:
      autotune(binary, candidates)
      
      #Candidate has not failed: mark as such
      candidate = candidates[-1]
      
    except tunerwarnings.AlwaysCrashes:
      print "Candidate "+str(count)+" always crashes!"
      return learningframework.FailedCandidate(hSet, assignScores = True)
      
    #Store the actually used hSet inside the candidate
    infoFile = os.path.join(basesubdir, 
                            str(count), 
                            basename+".info")
    hSet = learningframework.HeuristicSet()
    hSet.importFromXml(infoFile)
   
    candidate.heuristicSet = hSet
    candidate.failed = False
    candidate.assignScores = True
   
    return candidate
  

    
  def setup(self, benchmark, additionalParameters):
    candidates = additionalParameters["candidates"]
    
    #Define file names
    path, basenameExt = os.path.split(benchmark)
    if path == "":
      path="./"
    basename, ext = os.path.splitext(basenameExt)
    basesubdir = os.path.join(path,basename+".tmp")
    additionalParameters["basesubdir"] = basesubdir
    additionalParameters["basename"] = basename
    additionalParameters["path"] = path
    
    #Compile with default heuristics
    outDir = os.path.join(basesubdir, "0")
    if not os.path.isdir(outDir):
      #Create the output directory
      os.makedirs(outDir)
    binary= os.path.join(outDir, basename)  
    status = pbutil.compileBenchmark(self._pbcExe, 
                                   benchmark, 
                                   binary = binary, 
                                   jobs = self._jobs, 
                                   defaultHeuristics = True)  
    if status != 0:
      print "Compile FAILED with default heuristics - Compilation aborted"
      return status
      
    try:
      autotune(binary, candidates)
      
      #Candidate has not failed: mark as such
      currentCandidate = candidates[-1]
      currentCandidate.failed = False
      currentCandidate.assignScores = True

      
    except tunerwarnings.AlwaysCrashes:
      print "Compilation with default heuristics always crashes!"
      #Add an empty entry for the candidate
      currentCandidate = learningframework.FailedCandidate()
      candidates.append(currentCandidate)
    
    #Get the full set of used heuristics
    infoFile = binary+".info"
    currentDefaultHSet = learningframework.HeuristicSet()
    currentDefaultHSet.importFromXml(infoFile)
    currentCandidate.heuristicSet = currentDefaultHSet

    #Store the list of needed heuristics for the current benchmark
    self._neededHeuristics[benchmark] = currentDefaultHSet.keys() 
      
    return 0
    
  
  def getNeededHeuristics(self, benchmark):
    return self._neededHeuristics[benchmark]
    
    
  def tearDown(self, benchmark, additionalParameters):
    candidates = additionalParameters["candidates"]
    basesubdir = additionalParameters["basesubdir"]
    basename = additionalParameters["basename"]
    path = additionalParameters["path"]
    
    bestIndex = candidates[0].originalIndex
    print "The best candidate is: "+str(bestIndex)
  
    #Move every file to the right place
    bestSubDir = os.path.join(basesubdir, str(bestIndex))
    #  compiled program:
    bestBin = os.path.join(bestSubDir, basename)
    if self._finalBinary is not None:
      finalBin = self._finalBinary
    else:
      finalBin = os.path.join(path, basename)
    shutil.move(bestBin, finalBin)
    #  .cfg file
    bestCfg = os.path.join(bestSubDir, basename+".cfg")
    finalCfg = finalBin + ".cfg"
    shutil.move(bestCfg, finalCfg)
    #  .info file
    bestInfo = os.path.join(bestSubDir, basename+".info")
    finalInfo = finalBin+".info"
    shutil.move(bestInfo, finalInfo)
    #  .obj directory
    bestObjDir = os.path.join(bestSubDir, basename+".obj")
    destObjDir = finalBin+".obj"
    if os.path.isdir(destObjDir):
      shutil.rmtree(destObjDir)
    shutil.move(bestObjDir, destObjDir)
    #  input heuristic file
    if bestIndex != 0: #Program 0 is run with only the best heuristics in the DB
      bestHeurFile = os.path.join(bestSubDir, CONF_HEURISTIC_FILE_NAME)
      finalHeurFile = finalBin+".heur"
      shutil.move(bestHeurFile, finalHeurFile)
    
    #Delete all the rest
    if CONF_DELETE_TEMP_DIR:
      shutil.rmtree(basesubdir)
      
      

      
      
#TEST
if __name__ == "__main__":
  #basedir="/afs/csail.mit.edu/u/m/mtartara/programs/petabricks/"
  basedir="/home/mikyt/programmi/petabricks/"
  pbc = basedir+"src/pbc"
  try:
    heuristics = sys.argv[2]
  except:
    heuristics = None
  l = LearningCompiler(pbc, heuristicSetFileName = heuristics)
  l.compileLearningHeuristics(sys.argv[1])
