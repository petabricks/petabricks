#!/usr/bin/python
import sys
import os
import shutil
from xml.dom.minidom import parse
from pbutil import compileBenchmark
from sgatuner import autotune
from tunerconfig import config

class HeuristicSet(dict):
  def toXmlStrings(self):
    return ["""<heuristic name="{0}" formula="{1}" />""".format(name, self[name]) for name in self]
  
  def toXmlFile(self, filename):
    outfile = open(filename, "w")
    for xmlstring in self.toXmlStrings():
      outfile.write(xmlstring)
      outfile.write("\n")
    outfile.close()
  
  def importFromXml(self, xmlDOM):
    for heuristicXML in xmlDOM.getElementsByTagName("heuristic"):
      name=heuristicXML.getAttribute("name")
      formula=heuristicXML.getAttribute("formula")
      self[name] = formula
  
    
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
  def __init__(self, heuristicSetFileName):
    self.__heuristicSets = []
    self.__xml = parse(heuristicSetFileName)
    
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
  def __init__(self, pbcExe, heuristicSetFileName, minTrialNumber=0):
    self.__heuristicManager = HeuristicManager(heuristicSetFileName)
    self.__minTrialNumber = 0
    self.__pbcExe = pbcExe  
    
  def compileCurrentBest(self, benchmark, subdir):
    """Compile the benchmark without specifying any heuristic, thus forcing the
compiler to generate the current best program"""
    compileBenchmarkInSubdif
    
  def compileBenchmarkInSubdir(self, benchmark, subdir, heuristics=None):
    """Compiles the given benchmark. The output is stored in the given 
subdirectory.

An optional HeuristicSet can be given as a parameter
""" 
    #Build file names
    path, filenameWithExt = os.path.split(benchmark)
    name, ext = os.path.splitext(filenameWithExt);
    
    outDir = os.path.join(path,subdir)
    if not os.path.isdir(outDir):
      os.makedirs(outDir)
    bin= os.path.join(outDir, name)
    heuristicsFile= os.path.join(outDir, "heuristics.txt")
    
    #Dump the heuristics to a file
    heuristics.toXmlFile(heuristicsFile)
    
    #Compile
    compileBenchmark(self.__pbcExe, benchmark, bin=bin, heuristics=heuristicsFile)
    
    
  def compileLearningHeuristics(self, benchmark):
    #Define file names
    path, basenameExt = os.path.split(benchmark)
    basename, ext = os.path.splitext(basenameExt);
    basesubdir=os.path.join(path,basename+".tmp")
    
    candidates=[]
    
    count=0
    for hSet in self.__heuristicManager.allHeuristicSets():
      #Define more file names
      outDir = os.path.join(basesubdir, str(count))
      if not os.path.isdir(outDir):
        #Create the output directory
        os.makedirs(outDir)
      binary= os.path.join(outDir, basename)  
      heuristicsFile= os.path.join(outDir, "heuristics.txt")
      
      hSet.toXmlFile(heuristicsFile)
      
      #TODO: complete the hSet
      
      compileBenchmark(self.__pbcExe, benchmark, binary=binary, heuristics=heuristicsFile)
      
      #Autotune
      config.max_time=1 #Seconds #TODO: make more flexible
      autotune(binary, candidates)
      
      count = count + 1
      
    #Get the number of dimensions available for all candidates
    dimensions = len(candidates[0].metrics[0])
    for candidate in candidates:
      dimensions = min(len(candidate.metrics[0]), dimensions)
      
    #Select best candidate (only looking at the biggest shared dimensions)
    scores = []
    for candidate in candidates:
      timingResultDB = candidate.metrics[0] #Get the 'timings' metric
      #The score for each candidate is the average timing on the highest shared 
      #dimension
      results = timingResultDB[2**(dimensions-1)]
      scores.append(float(results.mean()))
      
    minScore = scores[0]
    p=0
    bestIndex=0
    for s in scores:
      if s<minScore:
        minScore=s
        bestIndex=p
      p=p+1
    
    print "The best candidate is: {0}".format(bestIndex)
  
    #Move every file to the right place
    bestSubDir=os.path.join(basesubdir, str(bestIndex))
    #  compiled program:
    bestBin=os.path.join(bestSubDir, basename)
    finalBin=os.path.join(path, basename)
    shutil.move(bestBin, finalBin)
    #  .cfg file
    bestCfg=os.path.join(bestSubDir, basename+".cfg")
    finalCfg=os.path.join(path, basename+".cfg")
    shutil.move(bestCfg, finalCfg)
    #  .obj directory
    bestObjDir=os.path.join(bestSubDir, basename+".obj")
    destObjDir=os.path.join(path, basename+".obj")
    if os.path.isdir(destObjDir):
      shutil.rmtree(destObjDir)
    shutil.move(bestObjDir, path)
    #  input heuristic file
    bestHeurFile=os.path.join(bestSubDir, "heuristics.txt")
    finalHeurFile=os.path.join(path, basename+".heur")
    shutil.move(bestHeurFile, finalHeurFile)
    
    #Delete all the rest
    shutil.rmtree(basesubdir)

#TEST
if __name__ == "__main__":
  basedir="/home/mikyt/programmi/petabricks/"
  pbc=basedir+"src/pbc"
  l=LearningCompiler(pbc, sys.argv[1])
  l.compileLearningHeuristics(sys.argv[2])
