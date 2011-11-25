import sqlite3
import os

class HeuristicDB:
  def __init__(self):
    #Open DB    
    try:
      self.__db = sqlite3.connect(self.computeDBPath())
    except:
      self.__db = sqlite3.connect(":memory:")
    self.__createTables()
    self.__createView("HeuristicsRank", 
          "SELECT *, "
          "Heuristic.score/Heuristic.useCount as finalScore "
          "FROM Heuristic "
          "ORDER BY Heuristic.score/Heuristic.useCount")
    self.__bestNCache= dict()
    
  def __createTable(self, name, params):
    cur = self.__db.cursor()
    query = "CREATE TABLE IF NOT EXISTS '"+name+"' "+params
    cur.execute(query)
    cur.close()
    self.__db.commit()
    
  def __createView(self, name, params):
    cur = self.__db.cursor()
    query = "CREATE VIEW IF NOT EXISTS '"+name+"' AS "+params
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
    dbPath= os.path.expanduser("~/tunerout/knowledge.db")
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
