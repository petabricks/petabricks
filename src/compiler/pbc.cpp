/***************************************************************************
 *   Copyright (C) 2008 by Jason Ansel                                     *
 *   jansel@csail.mit.edu                                                  *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/

#include "codegenerator.h"
#include "transform.h"

#include "common/jargs.h"
#include "common/jfilesystem.h"

#include <fstream>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

void callCxxCompiler();
std::string cmdCxxCompiler();

using namespace petabricks;

//do dynamic searches for the correct paths to needed libraries/headers
static bool shouldCompile = true;
static bool shouldLink = true;
static std::string theCommonDir;
static std::string theInput;
static std::string theLibDir;
static std::string theMainName;
static std::string theObjectFile;
static std::string theOutputBin;
static std::string theOutputCode;
static std::string theOutputInfo;
static std::string theRuntimeDir;

//defined in pbparser.ypp
TransformListPtr parsePbFile(const char* filename);

int main( int argc, const char ** argv){
  std::vector<std::string> inputs;
  
  jalib::JArgs args(argc, argv, "input");
  if(args.needHelp())
    fprintf(stderr, "OPTIONS:\n");


  args.param("input",      inputs).help("input file to compile (*.pbcc)");
  args.param("output",     theOutputBin).help("output binary to be produced");
  args.param("outputcpp",  theOutputCode).help("output *.cpp file to be produced");
  args.param("outputinfo", theOutputInfo).help("output *.info file to be produced");
  args.param("outputobj",  theObjectFile).help("output *.o file to be produced");
  args.param("runtimedir", theRuntimeDir).help("directory where petabricks.h may be found");
  args.param("libdir",     theLibDir).help("directory where libpbruntime.a may be found");
  args.param("compile",    shouldCompile).help("set to 'no' to disable compilation step");
  args.param("link",       shouldLink).help("set to 'no' to disable linking step");
  args.param("main",       theMainName).help("transform name to use as program entry point");

  if(args.param("version").help("print out version number and exit") ){
    fprintf(stderr, PACKAGE " compiler (pbc) v" VERSION " " REVISION_LONG "\n");
    return 1;
  }

  if(inputs.empty() || args.needHelp()){
    fprintf(stderr, "\n" PACKAGE " compiler (pbc) v" VERSION " " REVISION_SHORT "\n");
    fprintf(stderr, "USAGE: %s [OPTIONS] filename.pbcc\n", argv[0]);
    fprintf(stderr, "run `%s --help` for options\n", argv[0]);
    return 1;
  }

  JASSERT(inputs.size()==1)(inputs.size()).Text("expected exactly one input file");
  theInput = inputs.front();
  if(theRuntimeDir.empty()) theRuntimeDir = jalib::Filesystem::Dirname(jalib::Filesystem::FindHelperUtility("runtime/petabricks.h"));
  if(theLibDir    .empty()) theLibDir     = jalib::Filesystem::Dirname(jalib::Filesystem::FindHelperUtility("libpbruntime.a"));
  if(theOutputBin .empty()) theOutputBin  = jalib::Filesystem::Basename(theInput);
  if(theOutputCode.empty()) theOutputCode = theOutputBin + ".cpp";
  if(theOutputInfo.empty()) theOutputInfo = theOutputBin + ".info";
  if(theObjectFile.empty()) theObjectFile = theOutputBin + ".o";

  CodeGenerator::theFilePrefix() << "// Generated by " PACKAGE " compiler (pbc) v" VERSION " " REVISION_LONG "\n";
  CodeGenerator::theFilePrefix() << "// Compile with:\n";
  CodeGenerator::theFilePrefix() << "// " << cmdCxxCompiler() << "\n\n";
  CodeGenerator::theFilePrefix() << "#include \"petabricks.h\"\n";
  #ifdef SHORT_TYPE_NAMES
  CodeGenerator::theFilePrefix() <<"using namespace petabricks;\n\n";
  #endif

  TransformListPtr t = parsePbFile(theInput.c_str());

  for(TransformList::iterator i=t->begin(); i!=t->end(); ++i){
    (*i)->initialize();
    #ifdef DEBUG
    (*i)->print(std::cout);
    #endif
  }

  for(TransformList::iterator i=t->begin(); i!=t->end(); ++i){
    (*i)->compile();
  }

  std::ofstream of(theOutputCode.c_str());
  std::ofstream infofile(theOutputInfo.c_str());
  MainCodeGenerator o;
  for(TransformList::iterator i=t->begin(); i!=t->end(); ++i){
    (*i)->generateCode(o);
  }

  //find the main transform if it has not been specified
  if(theMainName==""){
    for(TransformList::const_iterator i=t->begin(); i!=t->end(); ++i){
      if((*i)->isMain()){
        JASSERT(theMainName=="")(theMainName)((*i)->name())
          .Text("Two transforms both have the 'main' keyword");
        theMainName = (*i)->name();
      }
    }
    if(theMainName=="") theMainName = t->back()->name();
  }

  o.comment("A hook called by PetabricksRuntime");
  o.beginFunc("petabricks::PetabricksRuntime::Main*", "petabricksMainTransform");
  o.write("return "+theMainName+"_main::instance();");
  o.endFunc();
  
  o.comment("A hook called by PetabricksRuntime");
  o.beginFunc( "petabricks::PetabricksRuntime::Main*"
             , "petabricksFindTransform"
             , std::vector<std::string>(1, "const std::string& name"));
  for(TransformList::iterator i=t->begin(); i!=t->end(); ++i){
    (*i)->registerMainInterface(o);
  }
  o.write("return NULL;");
  o.endFunc();
  
  o.outputFileTo(of);
  of.flush();
  of.close();
  o.cg().dumpTo(infofile);
  infofile.flush();
  infofile.close();
  if(shouldCompile)
    callCxxCompiler();

  JTRACE("done")(theInput)(theOutputInfo)(theOutputCode)(theOutputBin);
  return 0;
}

std::string cmdCxxCompiler(){
  std::ostringstream os;
  os << "echo -n Calling C++ compiler...\\ && \\\n"
     << CXX " " CXXFLAGS " " CXXDEFS " -c -o " << theObjectFile << " " << theOutputCode << " -I\""<<theLibDir<<"\" -I\""<<theRuntimeDir<<"\""
     << " && \\\n echo done";
  if(shouldLink)
    os <<" && echo -n Linking...\\ && \\\n"
       << CXX " " CXXFLAGS " -o " << theOutputBin << " " << theObjectFile << " -L\""<< theLibDir <<"\" -lpbmain -lpbruntime -lpbcommon " CXXLIBS 
       << " && \\\n echo done";
  return os.str();
}

void callCxxCompiler(){
  std::string cmd = cmdCxxCompiler();
  JTRACE("Running g++")(cmd);
  //std::cout << cmd << std::endl;
  int rv = system(cmd.c_str());
  JASSERT(rv==0)(rv)(cmd).Text("g++ call failed");
}

