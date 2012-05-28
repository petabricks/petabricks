/*****************************************************************************
 *  Copyright (C) 2008-2011 Massachusetts Institute of Technology            *
 *                                                                           *
 *  Permission is hereby granted, free of charge, to any person obtaining    *
 *  a copy of this software and associated documentation files (the          *
 *  "Software"), to deal in the Software without restriction, including      *
 *  without limitation the rights to use, copy, modify, merge, publish,      *
 *  distribute, sublicense, and/or sell copies of the Software, and to       *
 *  permit persons to whom the Software is furnished to do so, subject       *
 *  to the following conditions:                                             *
 *                                                                           *
 *  The above copyright notice and this permission notice shall be included  *
 *  in all copies or substantial portions of the Software.                   *
 *                                                                           *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY                *
 *  KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE               *
 *  WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND      *
 *  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE   *
 *  LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION   *
 *  OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION    *
 *  WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE           *
 *                                                                           *
 *  This source code is part of the PetaBricks project:                      *
 *    http://projects.csail.mit.edu/petabricks/                              *
 *                                                                           *
 *****************************************************************************/

#include "dbmanager.h"
#include "common/jfilesystem.h"
#include <fstream>

namespace {
  /** Multiprocess-safe Wrapper of sqlite3_exec: if a query fails because 
   * a table is blocked by another process, it is tried again for some times */
  static int db_exec(sqlite3* db, const char *query, int (*callback)(void*,int,char**,char**), void * argument, char **errmsg) {
    int retCode;
    int retry = 500; //Number of trials before aborting
    
    retCode = sqlite3_exec(db, query, callback, argument, errmsg);
    retry--;
    
    while ((retCode == SQLITE_LOCKED || retCode == SQLITE_BUSY) && --retry) {
      retCode = sqlite3_exec(db, query, callback, argument, errmsg);
      usleep(200*1000);
    }
    
    return retCode;
  }
  
  static int getHeuristic(void* result, int, char** colText, char**) {
    char* formula=colText[0];
    petabricks::Heuristic* newHeuristic = new petabricks::Heuristic(formula);
    
    *((petabricks::HeuristicPtr *)result) = newHeuristic;
    return 0;
  }
}

petabricks::DBManager::DBManager(std::string dbFileName) : emptyDB(false) {
  int retCode;

  if(dbFileName=="") {
    dbFileName=defaultDBFileName();
  }
  
  //Check if DB exists
  if(! jalib::Filesystem::FileExists(dbFileName)) {
    emptyDB = true;
  }
    
  //Open DB
  retCode = sqlite3_open(dbFileName.c_str(), &_db);

  if(retCode && emptyDB) {
    retCode = sqlite3_open(":memory:", &_db);
  }

  if(retCode) {
    std::cerr << "Can't open DB: " << dbFileName << "\n";
    sqlite3_close(_db);
    abort();
  }
}


std::string petabricks::DBManager::defaultDBFileName() {
  char* homeDir = getenv("HOME");
  return std::string(homeDir) + "/tunerout/knowledge.db";
}


petabricks::HeuristicPtr petabricks::DBManager::getBestHeuristic(std::string name) {
  int retCode;
  char *zErrMsg = 0;
  HeuristicPtr result;
  
  if(emptyDB) {
    //Return empty result
    return result;
  }
  
  std::string query = "SELECT formula FROM Heuristic JOIN HeuristicKind "
                      "ON Heuristic.kindID=HeuristicKind.ID "
                      "WHERE HeuristicKind.name='"+ name + "' "
                      "ORDER BY Heuristic.score/Heuristic.useCount DESC "
                      "LIMIT 1";
                      
  retCode = db_exec(_db, query.c_str(), getHeuristic, &result, &zErrMsg);
  if (retCode) {
    std::cerr << "Error getting the required heuristic: (" << retCode << ") " << zErrMsg << "\n";
    sqlite3_free(zErrMsg);
    sqlite3_close(_db);
    abort();
  }
  
  return result;
}
